(ns sqlite-cljs.dsl-test
  "Pure renderer tests: exact SQL strings and parameter vectors, no database."
  (:require [cljs.test :refer [deftest is testing]]
            [clojure.string :as str]
            [sqlite-cljs.dsl :as dsl]
            [sqlite-cljs.util :as util]))

(defn- err
  "The ex-data of the error thrown by (thunk), or nil."
  [thunk]
  (try (thunk) nil (catch :default e (ex-data e))))

(defn- dsl-error? [thunk]
  (= :sqlite-cljs/dsl-error (:type (err thunk))))

(deftest select-basics
  (is (= {:sql "SELECT * FROM \"users\"" :params []} (dsl/render {:select :* :from :users})))
  (is (= {:sql "SELECT \"id\", \"user_name\" FROM \"users\"" :params []}
         (dsl/render {:select [:id :user-name] :from :users})))
  (is (= "SELECT DISTINCT \"org_id\" FROM \"users\""
         (:sql (dsl/render {:select [:org-id] :from :users :distinct? true}))))
  (is (= "SELECT \"u\".\"id\", \"u\".* FROM \"users\" AS \"u\""
         (:sql (dsl/render {:select [:u/id :u/*] :from [:users :u]}))))
  (is (= "SELECT \"Exact Name\" FROM \"Mixed Case\""
         (:sql (dsl/render {:select ["Exact Name"] :from "Mixed Case"})))
      "strings in identifier positions are exact identifiers"))

(deftest select-aliases-and-functions
  (is (= "SELECT \"email\" AS \"contact\", count(*) AS \"n\", lower(\"name\") AS \"lname\", lower(\"name\"), upper(\"name\") AS \"u\" FROM \"t\""
         (:sql (dsl/render {:select [[:email :contact] [[:count :*] :n] [[:lower :name] :lname] [[:lower :name]] [:as [:upper :name] :u]]
                            :from :t})))
      "alias pairs, wrapped calls and [:as ..]")
  (is (= "SELECT count(*) FROM \"t\"" (:sql (dsl/render {:select [[:count :*]] :from :t})))
      "[:count :*] is the one 2-vector that is a call, not an alias")
  (is (= "SELECT count(DISTINCT \"org_id\") AS \"orgs\", group_concat(\"name\", ?) AS \"names\", total(\"n\") AS \"t\", coalesce(\"a\", \"b\", ?) AS \"c\" FROM \"t\""
         (:sql (dsl/render {:select [[[:count [:distinct :org-id]] :orgs] [[:group-concat :name ","] :names]
                                     [[:total :n] :t] [[:coalesce :a :b 0] :c]]
                            :from :t})))
      "aggregates: count/sum/min/max/avg/total/group_concat are plain functions")
  (is (dsl-error? #(dsl/render {:select [[[:bad-fn! :x]]] :from :t})) "function names are [a-z0-9_] only")
  (is (= "SELECT \"lower\" AS \"name\" FROM \"t\"" (:sql (dsl/render {:select [[:lower :name]] :from :t})))
      "documented wart shared with honeysql: a bare [kw kw] is an alias pair (SQLite then fails loudly: no such column)")
  (is (dsl-error? #(dsl/render {:select [[:coalesce :* :b]] :from :t})) ":* only as the sole argument"))

(deftest duplicate-result-names-rejected
  (is (= {:duplicates ["id"]}
         (select-keys (err #(dsl/render {:select [:u/id :o/id] :from [:users :u]})) [:duplicates]))
      "the driver collapses duplicate keys; the renderer demands aliases")
  (is (dsl-error? #(dsl/render {:select [:name [:email :name]] :from :t})))
  (is (= "SELECT \"u\".\"id\" AS \"uid\", \"o\".\"id\" AS \"oid\" FROM \"users\" AS \"u\""
         (:sql (dsl/render {:select [[:u/id :uid] [:o/id :oid]] :from [:users :u]})))))

(deftest comparison-operators
  (let [r (dsl/render {:select [:id] :from :t
                       :where [:and [:= :a 1] [:not= :b "x"] [:<> :c 2] [:< :d 3] [:<= :e 4] [:> :f 5] [:>= :g 6]]})]
    (is (= "SELECT \"id\" FROM \"t\" WHERE (\"a\" = ? AND \"b\" <> ? AND \"c\" <> ? AND \"d\" < ? AND \"e\" <= ? AND \"f\" > ? AND \"g\" >= ?)"
           (:sql r)))
    (is (= [1 "x" 2 3 4 5 6] (:params r))))
  (is (= "SELECT \"id\" FROM \"t\" WHERE (\"deleted_at\" IS NULL OR \"x\" IS NOT NULL OR \"y\" IS NULL OR \"z\" IS NOT NULL)"
         (:sql (dsl/render {:select [:id] :from :t
                            :where [:or [:= :deleted-at nil] [:not= :x nil] [:is-null :y] [:not-null :z]]})))
      "nil comparisons become IS NULL / IS NOT NULL")
  (is (= {:sql "SELECT \"id\" FROM \"t\" WHERE NOT (\"active\" = ?)" :params [true]}
         (dsl/render {:select [:id] :from :t :where [:not [:= :active true]]}))
      "booleans stay booleans in :params (bound as 1/0 by core)")
  (is (= "SELECT \"id\" FROM \"t\" WHERE (\"a\" IS ? AND \"b\" IS NOT ? AND \"c\" IS DISTINCT FROM ?)"
         (:sql (dsl/render {:select [:id] :from :t :where [:and [:is :a 1] [:is-not :b 2] [:is-distinct-from :c 3]]})))))

(deftest logical-and-empty-forms
  (is (= "SELECT 1=1" (:sql (dsl/render {:select [[[:and]]]}))))
  (is (= "SELECT 0=1" (:sql (dsl/render {:select [[[:or]]]}))))
  (is (= "SELECT \"id\" FROM \"t\" WHERE \"a\" = ?" (:sql (dsl/render {:select [:id] :from :t :where [:and [:= :a 1]]})))
      "single-arm :and collapses")
  (is (= "SELECT \"id\" FROM \"t\" WHERE ((\"a\" = ? OR \"b\" = ?) AND \"c\" = ?)"
         (:sql (dsl/render {:select [:id] :from :t :where [:and [:or [:= :a 1] [:= :b 2]] [:= :c 3]]})))))

(deftest in-lists
  (is (= {:sql "SELECT \"id\" FROM \"t\" WHERE \"id\" IN (?, ?, ?)" :params [1 2 3]}
         (dsl/render {:select [:id] :from :t :where [:in :id [1 2 3]]})))
  (is (= "SELECT \"id\" FROM \"t\" WHERE \"id\" NOT IN (?, ?)"
         (:sql (dsl/render {:select [:id] :from :t :where [:not-in :id ["a" "b"]]}))))
  (is (= "SELECT \"id\" FROM \"t\" WHERE 0=1" (:sql (dsl/render {:select [:id] :from :t :where [:in :id []]})))
      "empty IN is never true")
  (is (= "SELECT \"id\" FROM \"t\" WHERE 1=1" (:sql (dsl/render {:select [:id] :from :t :where [:not-in :id []]}))))
  (is (= "SELECT \"id\" FROM \"t\" WHERE \"id\" IN (SELECT \"user_id\" FROM \"grants\" WHERE \"ok\" = ?)"
         (:sql (dsl/render {:select [:id] :from :t :where [:in :id {:select [:user-id] :from :grants :where [:= :ok 1]}]})))
      "subquery IN"))

(deftest in-list-json-each-path
  (let [big (vec (range 501))
        r (dsl/render {:select [:id] :from :t :where [:in :id big]})]
    (is (= "SELECT \"id\" FROM \"t\" WHERE \"id\" IN (SELECT value FROM json_each(?))" (:sql r)))
    (is (= 1 (count (:params r))) "one JSON parameter instead of 501")
    (is (= "[0,1,2" (subs (first (:params r)) 0 6))))
  (is (= 500 (count (:params (dsl/render {:select [:id] :from :t :where [:in :id (vec (range 500))]}))))
      "500 values stay inline (threshold is > 500)")
  (is (= 1 (count (:params (dsl/render {:select [:id] :from :t :where [:in :id (vec (range 10))]}
                                       {:in-json-threshold 5}))))
      "threshold override")
  (is (= {:sql "SELECT \"id\" FROM \"t\" WHERE \"id\" IN (SELECT value FROM json_each(?))" :params ["[\"a\",\"b\"]"]}
         (dsl/render {:select [:id] :from :t :where [:in :id [:json-each ["a" "b"]]]}))
      "explicit [:json-each coll] forces the path")
  (is (dsl-error? #(dsl/render {:select [:id] :from :t :where [:in :id [:json-each [(js/BigInt 1)]]]}))
      "non-JSON values are refused"))

(deftest like-glob-between-exists
  (let [r (dsl/render {:select [:id] :from :t
                       :where [:and [:like :name "a%"] [:not-like :name "b%"] [:glob :path "*.txt"]
                               [:like :code "10\\%" {:escape "\\"}]
                               [:between :age 18 65] [:not-between :n 1 2]
                               [:exists {:select [1] :from :g :where [:= :g/uid :t/id]}]
                               [:not-exists {:select [1] :from :h}]]})]
    (is (= (str "SELECT \"id\" FROM \"t\" WHERE (\"name\" LIKE ? AND \"name\" NOT LIKE ? AND \"path\" GLOB ? AND \"code\" LIKE ? ESCAPE ?"
                " AND \"age\" BETWEEN ? AND ? AND \"n\" NOT BETWEEN ? AND ?"
                " AND EXISTS (SELECT ? FROM \"g\" WHERE \"g\".\"uid\" = \"t\".\"id\") AND NOT EXISTS (SELECT ? FROM \"h\"))")
           (:sql r)))
    (is (= ["a%" "b%" "*.txt" "10\\%" "\\" 18 65 1 2 1 1] (:params r)))))

(deftest arithmetic-cast-case-collate
  (is (= {:sql "SELECT ((\"a\" + ?) * ?) AS \"v\", (\"x\" || ?) AS \"s\", -(\"n\") AS \"neg\", ((\"p\" / \"q\") % ?) AS \"m\" FROM \"t\""
          :params [1 2 "!" 3]}
         (dsl/render {:select [[[:* [:+ :a 1] 2] :v] [[:|| :x "!"] :s] [[:- :n] :neg] [[:% [:/ :p :q] 3] :m]] :from :t})))
  (is (= "SELECT CAST(\"a\" AS INTEGER) AS \"i\", CAST(? AS TEXT) AS \"s\" FROM \"t\""
         (:sql (dsl/render {:select [[[:cast :a :integer] :i] [[:cast 1 :text] :s]] :from :t}))))
  (is (dsl-error? #(dsl/render {:select [[[:cast :a :boolean] :i]] :from :t})))
  (is (= {:sql "SELECT CASE WHEN \"a\" > ? THEN ? WHEN \"a\" > ? THEN ? ELSE ? END AS \"size\" FROM \"t\""
          :params [10 "big" 5 "mid" "small"]}
         (dsl/render {:select [[[:case [[:> :a 10] "big"] [[:> :a 5] "mid"] :else "small"] :size]] :from :t})))
  (is (= "SELECT \"id\" FROM \"t\" WHERE \"name\" COLLATE NOCASE = ?"
         (:sql (dsl/render {:select [:id] :from :t :where [:= [:collate :name :nocase] "x"]}))))
  (is (= "SELECT (\"data\" ->> ?) AS \"role\" FROM \"t\""
         (:sql (dsl/render {:select [[[:->> :data "$.role"] :role]] :from :t}))))
  (is (= :sqlite-cljs/dsl-error
         (:type (err #(dsl/render {:select [[[:->> :data "$.role"] :role]] :from :t} {:sqlite-version "3.37.0"}))))
      "->> is version gated when the version is known"))

(deftest raw-and-param
  (is (= {:sql "SELECT \"id\" FROM \"t\" WHERE json_extract(data, '$.role') = ?" :params ["admin"]}
         (dsl/render {:select [:id] :from :t :where [:raw "json_extract(data, '$.role') = ?" "admin"]})))
  (is (= {:sql "SELECT ? AS \"v\"" :params ["x"]} (dsl/render {:select [[[:param "x"] :v]]})))
  (is (dsl-error? #(dsl/render {:select [[[:param [1 2]] :v]]})) "[:param] still needs a bindable value")
  (is (dsl-error? #(dsl/render {:select [:id] :from :t :where [:= :a [1 2]]})) "collections are never auto-serialised")
  (is (dsl-error? #(dsl/render {:select [:id] :from :t :where [:= :a (js/Date.)]})) "js/Date refused")
  (is (dsl-error? #(dsl/render {:select [:id] :from :t :where [:= :a :b/c :d]})) "arity checked"))

(deftest joins
  (is (= "SELECT \"u\".\"id\", \"o\".\"name\" FROM \"users\" AS \"u\" INNER JOIN \"orgs\" AS \"o\" ON \"o\".\"id\" = \"u\".\"org_id\" LEFT JOIN \"profiles\" AS \"p\" USING (\"user_id\")"
         (:sql (dsl/render {:select [:u/id :o/name] :from [:users :u]
                            :join [[:orgs :o] [:= :o/id :u/org-id]]
                            :left-join [[:profiles :p] [:using :user-id]]}))))
  (is (= "SELECT \"a\".\"x\" FROM \"a\" LEFT JOIN \"b\" ON \"b\".\"a_id\" = \"a\".\"id\" RIGHT JOIN \"c\" AS \"cc\" ON \"cc\".\"id\" = \"b\".\"c_id\" FULL JOIN \"d\" ON \"d\".\"id\" = \"a\".\"d_id\" CROSS JOIN \"e\" INNER JOIN \"f\" ON \"f\".\"id\" = \"a\".\"f_id\""
         (:sql (dsl/render {:select [:a/x] :from :a
                            :joins [[:left-join :b [:= :b/a-id :a/id]]
                                    [:right-join [:c :cc] [:= :cc/id :b/c-id]]
                                    [:full-join :d [:= :d/id :a/d-id]]
                                    [:cross-join :e]
                                    [:inner-join :f [:= :f/id :a/f-id]]]})))
      ":joins keeps the written order")
  (is (dsl-error? #(dsl/render {:select [:x] :from :a :left-join [:b]})) "a non-cross join needs a condition")
  (is (= :sqlite-cljs/dsl-error
         (:type (err #(dsl/render {:select [:x] :from :a :joins [[:right-join :b [:= :b/id :a/id]]]} {:sqlite-version "3.38.0"}))))
      "RIGHT JOIN gated below 3.39")
  (is (= "SELECT \"s\".\"n\" FROM (SELECT count(*) AS \"n\" FROM \"t\") AS \"s\", \"u\""
         (:sql (dsl/render {:select [:s/n] :from [[{:select [[[:count :*] :n]] :from :t} :s] :u]})))
      "subquery in FROM plus a comma join"))

(deftest group-having-order-limit
  (let [r (dsl/render {:select [:org-id [[:count :*] :n]] :from :users
                       :where [:= :active true]
                       :group-by [:org-id]
                       :having [:> [:count :*] 1]
                       :order-by [[:n :desc] :org-id [[:lower :email] :asc :nulls-last] [:x [:collate :nocase]]]
                       :limit 10 :offset 20})]
    (is (= "SELECT \"org_id\", count(*) AS \"n\" FROM \"users\" WHERE \"active\" = ? GROUP BY \"org_id\" HAVING count(*) > ? ORDER BY \"n\" DESC, \"org_id\", lower(\"email\") ASC NULLS LAST, \"x\" COLLATE NOCASE LIMIT ? OFFSET ?"
           (:sql r)))
    (is (= [true 1 10 20] (:params r))))
  (is (= "SELECT \"id\" FROM \"t\" ORDER BY \"name\"" (:sql (dsl/render {:select [:id] :from :t :order-by :name}))))
  (is (dsl-error? #(dsl/render {:select [:id] :from :t :offset 5})) "OFFSET needs LIMIT")
  (is (dsl-error? #(dsl/render {:select [:id] :from :t :order-by [[:name :sideways]]})))
  (is (dsl-error? #(dsl/render {:select [:id] :from :t :order-by [[:lower :email]]})) "expression calls must be wrapped")
  (is (= "SELECT \"id\" FROM \"t\" ORDER BY lower(\"email\")" (:sql (dsl/render {:select [:id] :from :t :order-by [[[:lower :email]]]})))))

(deftest subqueries-nest
  (is (= {:sql "SELECT \"id\", (SELECT count(*) FROM \"orders\" WHERE \"orders\".\"uid\" = \"u\".\"id\") AS \"orders\" FROM \"users\" AS \"u\" WHERE \"org_id\" = (SELECT \"id\" FROM \"orgs\" WHERE \"name\" = ?)"
          :params ["acme"]}
         (dsl/render {:select [:id [{:select [[:count :*]] :from :orders :where [:= :orders/uid :u/id]} :orders]]
                      :from [:users :u]
                      :where [:= :org-id {:select [:id] :from :orgs :where [:= :name "acme"]}]}))
      "a map in an expression position is a subquery"))

(deftest ctes-and-unions
  (is (= {:sql "WITH \"recent\" AS (SELECT \"id\" FROM \"orders\" WHERE \"created_at\" > ?) SELECT \"id\" FROM \"recent\""
          :params [1700000000000]}
         (dsl/render {:with [[:recent {:select [:id] :from :orders :where [:> :created-at 1700000000000]}]]
                      :select [:id] :from :recent})))
  (is (= "WITH \"r\" AS (SELECT ?) SELECT * FROM \"r\""
         (:sql (dsl/render {:with {:r {:select [1]}} :select :* :from :r})))
      "map form")
  (is (= {:sql "WITH RECURSIVE \"c\"(\"x\") AS (SELECT ? UNION ALL SELECT (\"x\" + ?) FROM \"c\" WHERE \"x\" < ?) SELECT \"x\" FROM \"c\""
          :params [1 1 3]}
         (dsl/render {:with [[[:c [:x]] {:union-all [{:select [1]} {:select [[:+ :x 1]] :from :c :where [:< :x 3]}]} {:recursive true}]]
                      :select [:x] :from :c})))
  (is (= "WITH \"m\" AS MATERIALIZED (SELECT ?) SELECT * FROM \"m\""
         (:sql (dsl/render {:with [[:m {:select [1]} {:materialized true}]] :select :* :from :m}))))
  (is (= {:sql "SELECT \"id\" FROM \"a\" UNION SELECT \"id\" FROM \"b\" ORDER BY \"id\" DESC LIMIT ?" :params [5]}
         (dsl/render {:union [{:select [:id] :from :a} {:select [:id] :from :b}] :order-by [[:id :desc]] :limit 5})))
  (is (= "SELECT \"id\" FROM \"a\" INTERSECT SELECT \"id\" FROM \"b\" EXCEPT SELECT \"id\" FROM \"c\""
         (:sql (dsl/render {:except [{:intersect [{:select [:id] :from :a} {:select [:id] :from :b}]} {:select [:id] :from :c}]}))))
  (is (dsl-error? #(dsl/render {:union [{:select [:id] :from :a :limit 1} {:select [:id] :from :b}]}))
      "members cannot carry ORDER BY/LIMIT"))

(deftest window-functions
  (is (= "SELECT row_number() OVER (PARTITION BY \"org_id\" ORDER BY \"created_at\" DESC) AS \"rn\" FROM \"t\""
         (:sql (dsl/render {:select [[[:over [:row-number] {:partition-by [:org-id] :order-by [[:created-at :desc]]}] :rn]] :from :t}))))
  (is (dsl-error? #(dsl/render {:select [[[:over [:row-number] {:frame :rows}] :rn]] :from :t})) "frames are [:raw] territory"))

(deftest insert-forms
  (is (= {:sql "INSERT INTO \"users\" (\"name\", \"email\") VALUES (?, ?), (?, ?)" :params ["a" "a@x" "b" "b@x"]}
         (dsl/render {:insert-into :users :values [{:name "a" :email "a@x"} {:name "b" :email "b@x"}]})))
  (is (= {:sql "INSERT INTO \"users\" (\"name\", \"email\") VALUES (?, ?), (?, ?)" :params ["a" "a@x" "b" "b@x"]}
         (dsl/render {:insert-into :users :columns [:name :email] :values [["a" "a@x"] ["b" "b@x"]]})))
  (is (= "INSERT OR IGNORE INTO \"users\" (\"name\") VALUES (?)"
         (:sql (dsl/render {:insert-into :users :or :ignore :values [{:name "a"}]}))))
  (is (= "INSERT INTO \"users\" DEFAULT VALUES" (:sql (dsl/render {:insert-into :users :default-values true}))))
  (is (= {:sql "INSERT INTO \"users\" (\"name\") SELECT \"name\" FROM \"staging\" WHERE \"ok\" = ?" :params [1]}
         (dsl/render {:insert-into :users :columns [:name] :select {:select [:name] :from :staging :where [:= :ok 1]}})))
  (is (= {:sql "INSERT INTO \"users\" (\"id\", \"name\") VALUES (?, ?) RETURNING \"id\", \"created_at\"" :params [1 "a"]}
         (dsl/render {:insert-into :users :columns [:id :name] :values [[1 "a"]] :returning [:id :created-at]})))
  (is (= "INSERT INTO \"users\" (\"name\") VALUES (?) RETURNING *"
         (:sql (dsl/render {:insert-into :users :values [{:name "a"}] :returning :*}))))
  (is (= {:sql "INSERT INTO \"t\" (\"a\", \"b\") VALUES (?, ?)" :params [nil true]}
         (dsl/render {:insert-into :t :values [{:a nil :b true}]}))
      "nil and booleans are bound, never inlined")
  (is (dsl-error? #(dsl/render {:insert-into :users :values [{:name "a"} {:name "b" :email "x"}]}))
      "row key sets must match (no DEFAULT keyword in VALUES)")
  (is (dsl-error? #(dsl/render {:insert-into :users :values []})))
  (is (dsl-error? #(dsl/render {:insert-into :users :values [["a"]]})) "vector rows need :columns")
  (is (dsl-error? #(dsl/render {:insert-into :users :valuez [{:a 1}]})) "unknown clause"))

(deftest upsert-forms
  (is (= {:sql "INSERT INTO \"users\" (\"email\", \"name\") VALUES (?, ?) ON CONFLICT (\"email\") DO UPDATE SET \"name\" = \"excluded\".\"name\""
          :params ["a@x" "A"]}
         (dsl/render {:insert-into :users :values [{:email "a@x" :name "A"}] :on-conflict [:email] :do-update-set [:name]})))
  (is (= {:sql "INSERT INTO \"users\" (\"email\", \"name\", \"count\") VALUES (?, ?, ?) ON CONFLICT (\"email\") DO UPDATE SET \"count\" = (\"users\".\"count\" + ?), \"name\" = \"excluded\".\"name\" WHERE \"excluded\".\"updated_at\" > \"users\".\"updated_at\" RETURNING *"
          :params ["a@x" "A" 1 1]}
         (dsl/render {:insert-into :users :values [{:email "a@x" :name "A" :count 1}]
                      :on-conflict [:email]
                      :do-update-set {:count [:+ :users/count 1] :name :excluded/name}
                      :where [:> :excluded/updated-at :users/updated-at]
                      :returning :*})))
  (is (= "INSERT INTO \"users\" (\"email\") VALUES (?) ON CONFLICT DO NOTHING"
         (:sql (dsl/render {:insert-into :users :values [{:email "a@x"}] :on-conflict [] :do-nothing true}))))
  (is (= "INSERT INTO \"users\" (\"email\") VALUES (?) ON CONFLICT DO NOTHING"
         (:sql (dsl/render {:insert-into :users :values [{:email "a@x"}] :do-nothing true}))) "target optional")
  (is (= {:sql "INSERT INTO \"users\" (\"email\") VALUES (?) ON CONFLICT (\"email\") WHERE \"active\" = ? DO UPDATE SET \"email\" = \"excluded\".\"email\""
          :params ["a@x" 1]}
         (dsl/render {:insert-into :users :values [{:email "a@x"}] :on-conflict [[:email] {:where [:= :active 1]}] :do-update-set [:email]}))
      "partial-index conflict target")
  (is (= "INSERT INTO \"users\" (\"email\") VALUES (?) ON CONFLICT (\"email\") DO UPDATE SET \"email\" = \"excluded\".\"email\" ON CONFLICT DO NOTHING"
         (:sql (dsl/render {:insert-into :users :values [{:email "a@x"}]
                            :on-conflict [[[:email] {:do-update-set [:email]}] [[] {:do-nothing true}]]})))
      "multiple clauses")
  (is (= {:sql "INSERT INTO \"users\" (\"email\") SELECT \"email\" FROM \"staging\" WHERE true ON CONFLICT (\"email\") DO NOTHING" :params []}
         (dsl/render {:insert-into :users :columns [:email] :select {:select [:email] :from :staging} :on-conflict [:email] :do-nothing true}))
      "INSERT ... SELECT with an upsert gets WHERE true against the ON ambiguity")
  (is (dsl-error? #(dsl/render {:insert-into :users :values [{:email "a"}] :on-conflict [:email]})) "target without DO clause")
  (is (dsl-error? #(dsl/render {:insert-into :users :values [{:email "a"}] :where [:= :x 1]})) ":where alone is meaningless on insert"))

(deftest update-and-delete
  (is (= {:sql "UPDATE \"users\" SET \"name\" = ?, \"age\" = (\"age\" + ?), \"updated_at\" = unixepoch('subsec')*1000 WHERE \"id\" = ? RETURNING \"id\", \"name\""
          :params ["x" 1 42]}
         (dsl/render {:update :users :set {:name "x" :age [:+ :age 1] :updated-at [:raw "unixepoch('subsec')*1000"]}
                      :where [:= :id 42] :returning [:id :name]})))
  (is (= "UPDATE OR IGNORE \"users\" SET \"n\" = \"o\".\"n\" FROM \"orgs\" AS \"o\" WHERE \"o\".\"id\" = \"users\".\"org_id\""
         (:sql (dsl/render {:update :users :or :ignore :set {:n :o/n} :from [[:orgs :o]] :where [:= :o/id :users/org-id]})))
      "UPDATE FROM")
  (is (= "UPDATE \"t\" SET \"a\" = ?" (:sql (dsl/render {:update :t :set {:a 1} :all? true}))))
  (is (dsl-error? #(dsl/render {:update :t :set {:a 1}})) "WHERE-less update needs :all? true")
  (is (= {:sql "DELETE FROM \"users\" WHERE \"id\" = ? RETURNING *" :params [42]}
         (dsl/render {:delete-from :users :where [:= :id 42] :returning :*})))
  (is (= {:sql "DELETE FROM \"users\" WHERE \"created_at\" < ? RETURNING \"id\" ORDER BY \"created_at\" LIMIT ?" :params [0 100]}
         (dsl/render {:delete-from :users :where [:< :created-at 0] :order-by [:created-at] :limit 100 :returning [:id]}))
      "RETURNING precedes ORDER BY/LIMIT (verified against SQLite)")
  (is (dsl-error? #(dsl/render {:delete-from :t})) "WHERE-less delete needs :all? true")
  (is (= "DELETE FROM \"t\"" (:sql (dsl/render {:delete-from :t :all? true})))))

(deftest injection-safety
  (testing "values that look like SQL stay parameters"
    (let [evil "x'; DROP TABLE users; --"
          r (dsl/render {:select [:id] :from :users :where [:= :name evil]})]
      (is (= "SELECT \"id\" FROM \"users\" WHERE \"name\" = ?" (:sql r)))
      (is (= [evil] (:params r)))
      (is (not (str/includes? (:sql r) "DROP")))))
  (testing "identifiers that look like SQL are quoted and inert"
    (let [r (dsl/render {:select [:id] :from "users\"; DROP TABLE users; --"})]
      (is (= "SELECT \"id\" FROM \"users\"\"; DROP TABLE users; --\"" (:sql r)))))
  (testing "keywords in value positions are identifiers, never silently strings"
    (is (= "SELECT \"id\" FROM \"t\" WHERE \"status\" = \"active\".\"x_y\""
           (:sql (dsl/render {:select [:id] :from :t :where [:= :status :active/x-y]})))
        "a namespaced keyword is a qualified column reference — not a value")
    (is (= "SELECT \"id\" FROM \"t\" WHERE \"status\" = \"active\""
           (:sql (dsl/render {:select [:id] :from :t :where [:= :status :active]})))
        "plain keywords are identifiers (SQLite's DQS=0 makes a typo fail loudly)"))
  (testing "column values in insert maps are parameters even when they contain quotes"
    (is (= {:sql "INSERT INTO \"t\" (\"v\") VALUES (?)" :params ["it's \"quoted\""]}
           (dsl/render {:insert-into :t :values [{:v "it's \"quoted\""}]})))))

(deftest render-chunked-splits-bulk-inserts
  (let [rows (mapv (fn [i] {:a i :b (str i)}) (range 10))
        chunks (dsl/render-chunked {:insert-into :t :values rows} {:max-params 6})]
    (is (= 4 (count chunks)) "3 rows of 2 params per chunk")
    (is (every? #(<= (count (:params %)) 6) chunks))
    (is (= [0 "0" 1 "1" 2 "2"] (:params (first chunks))))
    (is (= [9 "9"] (:params (last chunks)))))
  (let [chunks (dsl/render-chunked {:insert-into :t :values [{:a 1 :b 2}] :on-conflict [:a] :do-update-set {:b [:+ :t/b 100]}}
                                   {:max-params 3})]
    (is (= 1 (count chunks)))
    (is (= [1 2 100] (:params (first chunks)))))
  (is (dsl-error? #(dsl/render-chunked {:insert-into :t :values [{:a 1 :b 2 :c 3}]} {:max-params 2})) "a row wider than the limit")
  (is (= 1 (count (dsl/render-chunked {:select [:id] :from :t}))) "non-inserts are one statement")
  (is (dsl-error? #(dsl/render {:select [:id] :from :t :where [:in :id (vec (range 40))]} {:max-params 10 :in-json-threshold 1000}))
      "a single statement above the parameter limit is refused"))

(deftest literal-mode
  (is (= "\"age\" >= 0" (dsl/render-literal [:>= :age 0])))
  (is (= "(\"name\" || ' <' || \"email\" || '>')" (dsl/render-literal [:|| :name " <" :email ">"])))
  (is (= "\"s\" = 'it''s'" (dsl/render-literal [:= :s "it's"])))
  (is (= "(\"a\" = 1 OR \"b\" IS NULL OR \"c\" = 1.5)" (dsl/render-literal [:or [:= :a true] [:= :b nil] [:= :c 1.5]])))
  (is (= {:sql "SELECT \"id\" FROM \"t\" WHERE \"active\" = 1"} (dsl/render {:select [:id] :from :t :where [:= :active 1]} {:mode :literal})))
  (is (dsl-error? #(dsl/render-literal [:raw "x = ?" 1])) "no parameters in literal mode")
  (is (dsl-error? #(dsl/render-literal [:in :id (vec (range 600))])) "json_each needs a parameter")
  (is (dsl-error? #(dsl/render-literal [:= :a (js/Uint8Array. 2)])) "blobs cannot be literals"))

(deftest statement-kind-and-errors
  (is (= :select (dsl/statement-kind {:select [:*]})))
  (is (= :select (dsl/statement-kind {:union [{:select [1]} {:select [2]}]})))
  (is (= :insert (dsl/statement-kind {:insert-into :t})))
  (is (= :update (dsl/statement-kind {:update :t})))
  (is (= :delete (dsl/statement-kind {:delete-from :t})))
  (is (nil? (dsl/statement-kind {:frob 1})))
  (is (dsl-error? #(dsl/render {:frob 1})))
  (is (dsl-error? #(dsl/render {:select [:id] :from :t :whre [:= :a 1]})) "typo'd clause")
  (is (= {:frob 1} (:form (err #(dsl/render {:frob 1})))) "dsl-errors carry the offending :form")
  (is (dsl-error? #(dsl/render [:select :*])) "not a map"))
