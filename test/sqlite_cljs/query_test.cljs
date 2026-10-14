(ns sqlite-cljs.query-test
  "Execute layer against :memory: databases: CRUD, upsert, transactions,
   statement cache, chunking, json_each, injection safety."
  (:require [cljs.test :refer [deftest is testing]]
            [promesa.core :as p]
            [sqlite-cljs.core :as core]
            [sqlite-cljs.query :as q]
            [sqlite-cljs.test-util :refer [rejection error-type] :refer-macros [deftest-p]]
            [sqlite-cljs.util :as util]))

(def users-def
  {:create-table :users
   :columns [[:id :integer {:primary-key? true}]
             [:email :text {:not-null? true :unique? true :collate :nocase}]
             [:name :text {:not-null? true}]
             [:age :integer {:check [:>= :age 0]}]
             [:active :integer {:not-null? true :default true}]
             [:org-id :integer {:references {:table :orgs :column :id :on-delete :cascade}}]
             [:created-at :integer {:not-null? true :default [:raw "(unixepoch('subsec') * 1000)"]}]]
   :indexes [[:users-org-idx [:org-id]]]})

(def orgs-def
  {:create-table :orgs
   :columns [[:id :integer {:primary-key? true}] [:name :text {:not-null? true :unique? true}]]})

(defn fresh-db
  "A :memory: db with orgs + users and two orgs."
  []
  (let [d (core/db)]
    (p/let [_ (q/exec! d [orgs-def users-def])
            _ (q/execute! d {:insert-into :orgs :values [{:id 1 :name "acme"} {:id 2 :name "globex"}]})]
      d)))

(deftest-p crud-round-trip
  (p/let [d (fresh-db)
          ins (q/execute! d {:insert-into :users :values [{:email "a@x" :name "Ann" :age 30 :org-id 1}
                                                          {:email "b@x" :name "Bob" :age 40 :org-id 2}]})
          all (q/query-all! d {:select [:id :email :name :age :active :org-id] :from :users :order-by [:id]})
          one (q/query-one! d {:select [:name] :from :users :where [:= :email "B@X"]})
          none (q/query-one! d {:select [:name] :from :users :where [:= :email "zz"]})
          cnt (q/query-value! d {:select [[[:count :*] :n]] :from :users})
          upd (q/execute! d {:update :users :set {:age [:+ :age 1]} :where [:= :name "Ann"]})
          age (q/query-value! d {:select [:age] :from :users :where [:= :name "Ann"]})
          del (q/execute! d {:delete-from :users :where [:= :name "Bob"] :returning [:id :name]})
          left (q/query-all! d {:select [:name] :from :users})]
    (is (= {:changes 2 :last-insert-rowid 2} ins))
    (is (= [{:id 1 :email "a@x" :name "Ann" :age 30 :active 1 :org-id 1}
            {:id 2 :email "b@x" :name "Bob" :age 40 :active 1 :org-id 2}] all)
        "kebab keys, INTEGER defaults, booleans as 1")
    (is (= {:name "Bob"} one) "COLLATE NOCASE column")
    (is (nil? none))
    (is (= 2 cnt))
    (is (= {:changes 1 :last-insert-rowid 2} upd))
    (is (= 31 age))
    (is (= [{:id 2 :name "Bob"}] del) "RETURNING makes execute! return rows")
    (is (= [{:name "Ann"}] left))
    (core/close! d)))

(deftest-p query-designators
  (p/let [d (fresh-db)
          a (q/query-all! d "SELECT name FROM orgs ORDER BY id")
          b (q/query-all! d ["SELECT name FROM orgs WHERE id = ?" 2])
          c (q/query-all! d {:sql "SELECT name FROM orgs WHERE id = ?" :params [1]})
          e (error-type (q/query-all! d 42))]
    (is (= [{:name "acme"} {:name "globex"}] a))
    (is (= [{:name "globex"}] b))
    (is (= [{:name "acme"}] c))
    (is (= :sqlite-cljs/argument-error e))
    (core/close! d)))

(deftest-p upsert-do-update-do-nothing-returning
  (p/let [d (fresh-db)
          r1 (q/execute! d {:insert-into :users :values [{:email "a@x" :name "Ann" :age 1 :org-id 1}]
                            :on-conflict [:email] :do-update-set [:name :age] :returning [:id :name :age]})
          r2 (q/execute! d {:insert-into :users :values [{:email "a@x" :name "Annie" :age 2 :org-id 1}]
                            :on-conflict [:email] :do-update-set [:name :age] :returning [:id :name :age]})
          r3 (q/execute! d {:insert-into :users :values [{:email "a@x" :name "Nobody" :age 3 :org-id 1}]
                            :on-conflict [:email] :do-nothing true})
          r4 (q/execute! d {:insert-into :users :values [{:email "a@x" :name "X" :age 99 :org-id 1}]
                            :on-conflict [:email]
                            :do-update-set {:age [:+ :users/age :excluded/age]}
                            :where [:< :users/age 50]
                            :returning [:age]})
          rows (q/query-all! d {:select [:id :name :age] :from :users})]
    (is (= [{:id 1 :name "Ann" :age 1}] r1))
    (is (= [{:id 1 :name "Annie" :age 2}] r2) "same id, updated columns")
    (is (= {:changes 0 :last-insert-rowid 1} r3) "DO NOTHING changes nothing")
    (is (= [{:age 101}] r4) "expression SET with excluded + DO UPDATE WHERE")
    (is (= [{:id 1 :name "Annie" :age 101}] rows))
    (core/close! d)))

(deftest-p insert-or-replace-is-a-delete
  (p/let [d (fresh-db)
          _ (q/execute! d {:insert-into :users :values [{:email "a@x" :name "Ann" :age 1 :org-id 1}]})
          r (q/execute! d {:insert-into :users :or :replace :values [{:email "a@x" :name "Ann2" :org-id 1}]})
          rows (q/query-all! d {:select [:id :name :age] :from :users})]
    (is (= 1 (:changes r)))
    (is (= [{:id 2 :name "Ann2" :age nil}] rows) "new rowid, unlisted columns reset (inventory §7.11)")
    (core/close! d)))

(deftest-p foreign-keys-cascade
  (p/let [d (fresh-db)
          _ (q/execute! d {:insert-into :users :values [{:email "a@x" :name "Ann" :org-id 1} {:email "b@x" :name "Bob" :org-id 2}]})
          [bad] (rejection (q/execute! d {:insert-into :users :values [{:email "c@x" :name "C" :org-id 99}]}))
          _ (q/execute! d {:delete-from :orgs :where [:= :id 1]})
          left (q/query-all! d {:select [:name] :from :users})]
    (is (= "SQLITE_CONSTRAINT_FOREIGNKEY" (util/sqlite-code bad)))
    (is (= [{:name "Bob"}] left) "ON DELETE CASCADE fired (foreign_keys on)")
    (core/close! d)))

(deftest-p transact-commits-and-returns-body-value
  (p/let [d (fresh-db)
          v (q/transact! d (fn [tx]
                             (is (core/in-transaction? tx))
                             (let [r @(q/execute! tx {:insert-into :users :values [{:email "a@x" :name "Ann" :org-id 1}]})]
                               @(q/execute! tx {:insert-into :users :values [{:email "b@x" :name "Bob" :org-id (:last-insert-rowid r)}]})
                               (:n @(q/query-one! tx {:select [[[:count :*] :n]] :from :users})))))
          n (q/query-value! d {:select [[[:count :*] :n]] :from :users})]
    (is (= 2 v) "deref'd settled promises give values inside the body")
    (is (= 2 n))
    (is (not (core/in-transaction? d)))
    (core/close! d)))

(deftest-p transact-rolls-back-on-throw-and-on-rejected-deref
  (p/let [d (fresh-db)
          [e1] (rejection (q/transact! d (fn [tx]
                                         @(q/execute! tx {:insert-into :users :values [{:email "a@x" :name "Ann" :org-id 1}]})
                                         (throw (ex-info "boom" {:my :data})))))
          n1 (q/query-value! d {:select [[[:count :*] :n]] :from :users})
          [e2] (rejection (q/transact! d (fn [tx]
                                         @(q/execute! tx {:insert-into :users :values [{:email "a@x" :name "Ann" :org-id 1}]})
                                         @(q/execute! tx {:insert-into :users :values [{:email "a@x" :name "Dup" :org-id 1}]}))))
          n2 (q/query-value! d {:select [[[:count :*] :n]] :from :users})
          [e3] (rejection (q/transact! d :deferred (fn [tx] (q/execute! tx {:insert-into :users :values [{:email "z@x" :name "Z" :org-id 1}]}))))
          n3 (q/query-value! d {:select [[[:count :*] :n]] :from :users})]
    (is (= "boom" (ex-message e1)))
    (is (= 0 n1) "explicit throw rolls back")
    (is (= "SQLITE_CONSTRAINT_UNIQUE" (util/sqlite-code e2)))
    (is (= 0 n2) "a rejected promise deref'd inside the body rolls back")
    (is (= :async-body (:reason (ex-data e3))) "returning a promise is refused")
    (is (= 0 n3) "…and rolled back")
    (is (not (core/in-transaction? d)))
    (core/close! d)))

(deftest-p transact-modes-and-nesting
  (p/let [d (fresh-db)
          a (q/transact! d :deferred (fn [_] :a))
          b (q/transact! d :exclusive (fn [_] :b))
          bad (error-type (q/transact! d :sideways (fn [_] :c)))
          [nested] (rejection (q/transact! d (fn [tx] @(q/transact! tx (fn [_] :inner)))))
          after (q/transact! d (fn [_] :ok))
          notfn (error-type (q/transact! d 42))]
    (is (= :a a))
    (is (= :b b))
    (is (= :sqlite-cljs/argument-error bad))
    (is (= :nested-transaction (:reason (ex-data nested))) "nested transact! rejects (and the outer rolls back)")
    (is (= :ok after) "connection usable afterwards")
    (is (= :sqlite-cljs/argument-error notfn))
    (core/close! d)))

(deftest-p execute-all-runs-in-one-transaction
  (p/let [d (fresh-db)
          res (q/execute-all! d [{:insert-into :users :values [{:email "a@x" :name "A" :org-id 1}]}
                                 ["INSERT INTO users (email, name, org_id) VALUES (?, ?, ?)" "b@x" "B" 1]
                                 {:create-index :users-name-idx :on :users :columns [:name]}
                                 {:select [[[:count :*] :n]] :from :users}])
          [bad] (rejection (q/execute-all! d [{:insert-into :users :values [{:email "c@x" :name "C" :org-id 1}]}
                                            {:insert-into :users :values [{:email "c@x" :name "C" :org-id 1}]}]))
          n (q/query-value! d {:select [[[:count :*] :n]] :from :users})]
    (is (= {:changes 1 :last-insert-rowid 1} (first res)))
    (is (= 1 (:changes (second res))))
    (is (= 1 (count (:statements (nth res 2)))))
    (is (= [{:n 2}] (nth res 3)))
    (is (util/constraint-error? bad))
    (is (= 2 n) "the failing batch left nothing behind")
    (core/close! d)))

(deftest-p exec-forms
  (p/let [d (core/db)
          _ (q/exec! d "CREATE TABLE a (x INTEGER) STRICT; INSERT INTO a VALUES (1); INSERT INTO a VALUES (2);")
          _ (q/exec! d ["CREATE TABLE b (y INTEGER) STRICT" {:create-index :b-y :on :b :columns [:y]}])
          _ (q/exec! d {:drop-table :b})
          n (q/query-value! d "SELECT count(*) AS n FROM a")
          tables (q/query-all! d "SELECT name FROM sqlite_master WHERE type = 'table' ORDER BY name")
          bad (error-type (q/exec! d [42]))]
    (is (= 2 n) "multi-statement scripts")
    (is (= [{:name "a"}] tables))
    (is (= :sqlite-cljs/argument-error bad))
    (core/close! d)))

(deftest-p statement-cache-lru
  (p/let [d (core/db {:memory? true :cache-size 3})
          _ (q/query-all! d "SELECT 1 AS a")
          _ (q/query-all! d "SELECT 1 AS a")
          _ (q/query-all! d "SELECT 2 AS a")
          _ (q/query-all! d "SELECT 3 AS a")
          size3 (p/resolved (core/cache-size d))
          hit (p/resolved (core/cached? d "SELECT 1 AS a"))
          _ (q/query-all! d "SELECT 1 AS a")
          _ (q/query-all! d "SELECT 4 AS a")
          evicted (p/resolved (core/cached? d "SELECT 2 AS a"))
          kept (p/resolved (core/cached? d "SELECT 1 AS a"))
          _ (q/exec! d "CREATE TABLE t (x INTEGER) STRICT")
          after-exec (p/resolved (core/cache-size d))
          _ (q/query-all! d "SELECT x FROM t")
          _ (q/clear-cache! d)
          cleared (p/resolved (core/cache-size d))]
    (is (= 3 size3) "one entry per distinct SQL text")
    (is hit)
    (is (not evicted) "least recently used entry evicted at capacity")
    (is kept "a hit refreshes recency")
    (is (= 0 after-exec) "exec! clears the cache (DDL may have run)")
    (is (= 0 cleared))
    (core/close! d)))

(deftest-p bulk-insert-chunks-in-one-transaction
  (p/let [d (core/db)
          _ (q/exec! d {:create-table :t :columns [[:a :integer {:not-null? true}] [:b :text {}]]})
          rows (mapv (fn [i] {:a i :b (str "r" i)}) (range 1000))
          r (q/execute! d {:insert-into :t :values rows} {:max-params 100})
          n (q/query-value! d {:select [[[:count :*] :n]] :from :t})
          sum (q/query-value! d {:select [[[:sum :a] :s]] :from :t})
          ret (q/execute! d {:insert-into :t :values (mapv (fn [i] {:a i :b nil}) (range 1000 1010)) :returning [:a]} {:max-params 4})
          [bad] (rejection (q/execute! d {:insert-into :t :values (conj rows {:a nil :b "boom"})} {:max-params 100}))
          n2 (q/query-value! d {:select [[[:count :*] :n]] :from :t})]
    (is (= 1000 (:changes r)) "changes summed over 20 chunks of 50 rows")
    (is (= 1000 n))
    (is (= (reduce + (range 1000)) sum))
    (is (= (mapv (fn [i] {:a i}) (range 1000 1010)) (sort-by :a ret)) "RETURNING rows concatenated across chunks")
    (is (= "SQLITE_CONSTRAINT_NOTNULL" (util/sqlite-code bad)))
    (is (= 1010 n2) "a failing chunk rolls back the whole bulk insert")
    (core/close! d)))

(deftest-p bulk-insert-above-real-limit
  (p/let [d (core/db)
          _ (q/exec! d {:create-table :t :columns [[:a :integer {}] [:b :integer {}] [:c :integer {}]]})
          rows (mapv (fn [i] {:a i :b i :c i}) (range 12000))
          r (q/execute! d {:insert-into :t :values rows})
          n (q/query-value! d {:select [[[:count :*] :n]] :from :t})]
    (is (= 12000 (:changes r)) "36000 parameters > 32766: chunked automatically")
    (is (= 12000 n))
    (core/close! d)))

(deftest-p huge-in-list-uses-json-each
  (p/let [d (core/db)
          _ (q/exec! d {:create-table :t :columns [[:id :integer {:primary-key? true}] [:s :text {}]]})
          _ (q/execute! d {:insert-into :t :values (mapv (fn [i] {:id i :s (str i)}) (range 1 2001))})
          ids (vec (range 1 1501))
          hits (q/query-all! d {:select [[[:count :*] :n]] :from :t :where [:in :id ids]})
          texts (q/query-value! d {:select [[[:count :*] :n]] :from :t :where [:in :s (mapv str (range 1 601))]})
          not-in (q/query-value! d {:select [[[:count :*] :n]] :from :t :where [:not-in :id ids]})
          explicit (q/query-value! d {:select [[[:count :*] :n]] :from :t :where [:in :id [:json-each [1 2 3]]]})]
    (is (= [{:n 1500}] hits) "1500 ids in one JSON parameter")
    (is (= 600 texts) "strings keep TEXT type through json_each")
    (is (= 500 not-in))
    (is (= 3 explicit))
    (core/close! d)))

(deftest-p joins-subqueries-ctes-unions-against-sqlite
  (p/let [d (fresh-db)
          _ (q/execute! d {:insert-into :users :values [{:email "a@x" :name "Ann" :age 30 :org-id 1}
                                                        {:email "b@x" :name "Bob" :age 40 :org-id 1}
                                                        {:email "c@x" :name "Cid" :age 50 :org-id 2}]})
          j (q/query-all! d {:select [:u/name [:o/name :org]] :from [:users :u]
                             :join [[:orgs :o] [:= :o/id :u/org-id]]
                             :where [:> :u/age 35] :order-by [:u/name]})
          lj (q/query-all! d {:select [:o/name [[:count :u/id] :n]] :from [:orgs :o]
                              :joins [[:left-join [:users :u] [:= :u/org-id :o/id]]]
                              :group-by [:o/id] :having [:>= [:count :u/id] 2] :order-by [[:n :desc]]})
          sub (q/query-all! d {:select [:name] :from :users
                               :where [:in :org-id {:select [:id] :from :orgs :where [:= :name "globex"]}]})
          cte (q/query-all! d {:with [[:old {:select [:name :age] :from :users :where [:>= :age 40]}]]
                               :select [:name] :from :old :order-by [[:age :desc]]})
          rec (q/query-all! d {:with [[[:c [:x]] {:union-all [{:select [1]} {:select [[:+ :x 1]] :from :c :where [:< :x 5]}]} {:recursive true}]]
                               :select [[[:sum :x] :total]] :from :c})
          un (q/query-all! d {:union [{:select [:name] :from :users :where [:= :age 30]}
                                      {:select [:name] :from :orgs :where [:= :id 2]}]
                              :order-by [:name]})
          win (q/query-all! d {:select [:name [[:over [:row-number] {:partition-by [:org-id] :order-by [[:age :desc]]}] :rn]]
                               :from :users :order-by [:name]})
          cs (q/query-all! d {:select [[[:case [[:< :age 35] "young"] :else "old"] :bucket] [[:cast :age :text] :s] [[:coalesce nil :age] :c]]
                              :from :users :where [:= :name "Ann"]})]
    (is (= [{:name "Bob" :org "acme"} {:name "Cid" :org "globex"}] j))
    (is (= [{:name "acme" :n 2}] lj))
    (is (= [{:name "Cid"}] sub))
    (is (= [{:name "Cid"} {:name "Bob"}] cte))
    (is (= [{:total 15}] rec))
    (is (= [{:name "Ann"} {:name "globex"}] un))
    (is (= [{:name "Ann" :rn 2} {:name "Bob" :rn 1} {:name "Cid" :rn 1}] win))
    (is (= [{:bucket "young" :s "30" :c 30}] cs))
    (core/close! d)))

(deftest-p injection-stays-data
  (p/let [d (fresh-db)
          evil "x'); DROP TABLE users; --"
          _ (q/execute! d {:insert-into :users :values [{:email evil :name "Mallory" :org-id 1}]})
          back (q/query-one! d {:select [:email] :from :users :where [:= :name "Mallory"]})
          again (q/query-one! d {:select [:name] :from :users :where [:= :email evil]})
          like (q/query-all! d {:select [:name] :from :users :where [:like :email "%DROP%"]})
          tbl (q/query-value! d {:select [[[:count :*] :n]] :from :sqlite-master :where [:= :name "users"]})
          raw (q/query-one! d ["SELECT ? AS v" "1; DROP TABLE users"])]
    (is (= {:email evil} back))
    (is (= {:name "Mallory"} again))
    (is (= [{:name "Mallory"}] like))
    (is (= 1 tbl) "users survived")
    (is (= {:v "1; DROP TABLE users"} raw))
    (core/close! d)))

(deftest-p delete-update-limit-and-update-from
  (p/let [d (fresh-db)
          _ (q/execute! d {:insert-into :users :values (mapv (fn [i] {:email (str i "@x") :name (str "u" i) :age i :org-id 1}) (range 10))})
          del (q/execute! d {:delete-from :users :where [:< :age 100] :order-by [[:age :desc]] :limit 3 :returning [:age]})
          upd (q/execute! d {:update :users :set {:name :o/name} :from [[:orgs :o]] :where [:= :o/id :users/org-id] :returning [:name]})
          n (q/query-value! d {:select [[[:count :*] :n]] :from :users})]
    (is (= #{9 8 7} (set (map :age del))) "ORDER BY .. LIMIT on DELETE (ENABLE_UPDATE_DELETE_LIMIT)")
    (is (every? #(= "acme" (:name %)) upd) "UPDATE FROM")
    (is (= 7 n))
    (core/close! d)))

(deftest ->statement-is-pure
  (is (= {:sql "SELECT 1" :params []} (q/->statement "SELECT 1")))
  (is (= {:sql "SELECT ?" :params [1]} (q/->statement ["SELECT ?" 1])))
  (is (= {:sql "SELECT ?" :params [2]} (q/->statement {:sql "SELECT ?" :params [2]})))
  (is (= {:sql "SELECT \"a\" FROM \"t\"" :params []} (q/->statement {:select [:a] :from :t})))
  (is (= :sqlite-cljs/argument-error (:type (try (q/->statement {:sql 1}) nil (catch :default e (ex-data e)))))))
