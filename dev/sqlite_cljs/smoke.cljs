(ns sqlite-cljs.smoke
  "LIVE smoke: the whole library against a REAL file database in a
   fresh temporary directory (os.tmpdir + mkdtemp, removed at the end) with
   an in-memory database opened side by side. No network anywhere; not
   part of run-tests.sh.

     npx shadow-cljs compile smoke && node target/smoke.js

   Structure (house pattern, see openrouter-cljs): numbered sections that
   never abort early. Each prints `PASS <name> — <detail>` or
   `FAIL <name>: <reason>`, the tally comes last, and the exit code is 0
   only when every section passed. A section that throws or rejects fails
   with the sqlite-cljs error type/code/reason and the suite continues.

   Sections: 0 open (versions, effective pragmas, file + :memory:),
   1 migrate! + idempotent re-run, 2 DSL inserts / upsert RETURNING / join,
   3 transactions (commit, rollback on throw, nested rejected),
   4 booleans + epoch-ms dates round trip, 5 bulk insert with chunking,
   6 drift and the user_version guard, 7 injection safety live,
   8 statement cache, 9 STRICT mode, 10 error taxonomy."
  (:require [clojure.string :as str]
            [promesa.core :as p]
            [sqlite-cljs.core :as core]
            [sqlite-cljs.ddl :as ddl]
            [sqlite-cljs.dsl :as dsl]
            [sqlite-cljs.migrate :as migrate]
            [sqlite-cljs.query :as q]
            [sqlite-cljs.util :as util]))

;; ---------------------------------------------------------------------------
;; Node modules (dev-only interop: no ^js hints, property/method access via
;; unchecked-get / js-invoke like sqlite-cljs.core)
;; ---------------------------------------------------------------------------

(def ^:private fs (js/require "node:fs"))
(def ^:private os (js/require "node:os"))
(def ^:private node-path (js/require "node:path"))

(defn- join-path [& parts] (apply js-invoke node-path "join" parts))
(defn- path-exists? [p] (boolean (js-invoke fs "existsSync" p)))
(defn- mkdtemp! [prefix] (js-invoke fs "mkdtempSync" (join-path (js-invoke os "tmpdir") prefix)))
(defn- rm-rf! [p] (js-invoke fs "rmSync" p #js {:recursive true :force true}))
(defn- now-ms [] (js/Date.now))

;; ---------------------------------------------------------------------------
;; Bookkeeping
;; ---------------------------------------------------------------------------

(def results
  "[{:name .. :ok? .. :reason ..} ...] in section order."
  (atom []))

(def shared
  "Cross-section values: :dir :path :file (db value) :mem (db value)."
  (atom {}))

(defn say [& xs] (println (str/join " " (map str xs))))

(defn error-brief
  "One line describing a rejection: the sqlite-cljs error type, SQLite
   code, :reason and message."
  [e]
  (let [d (or (util/error-data e) (ex-data e))]
    (if d
      (str (name (or (:type d) :error))
           (when (:code d) (str " code=" (:code d)))
           (when (:reason d) (str " reason=" (:reason d)))
           (when (:column d) (str " column=" (pr-str (:column d))))
           " — " (or (ex-message e) (:message d)))
      (or (ex-message e) (some-> e (unchecked-get "message")) (str e)))))

(defn section!
  "Runs `f` (a fn returning a promise of a detail string, or throwing /
   rejecting) as section `name`. Prints PASS/FAIL, records the result and
   ALWAYS resolves so the next section runs."
  [name f]
  (-> (p/do (f))
      (p/then (fn [detail]
                (say (str "PASS " name (when detail (str " — " detail))))
                (swap! results conj {:name name :ok? true})))
      (p/catch (fn [e]
                 (let [reason (error-brief e)]
                   (say (str "FAIL " name ": " reason))
                   (swap! results conj {:name name :ok? false :reason reason}))))))

(defn fail!
  "Throws a plain smoke failure with `reason`."
  [reason & [data]]
  (throw (ex-info reason (merge {:type :smoke/fail} data))))

(defn rejection
  "Resolves to [error] for a rejected promise, [nil] for a resolved one
   (boxed: promesa treats a bare error value as a rejection)."
  [promise]
  (-> promise (p/then (fn [_] [nil])) (p/catch (fn [e] [e]))))

(defn settled-error
  "The error of an already-rejected promise (nil otherwise); usable
   synchronously inside a transact! body."
  [promise]
  (when (p/rejected? promise) (p/extract promise)))

(defn expect-error!
  "Rejection `e` must be a sqlite-cljs error of `type` (and carry `code` /
   `reason` when given); returns its brief."
  [e what {:keys [type code reason]}]
  (when (nil? e) (fail! (str what ": expected a rejection, got success")))
  (when (and type (not= type (util/error-type e)))
    (fail! (str what ": expected " type ", got " (error-brief e))))
  (when (and code (not= code (util/sqlite-code e)))
    (fail! (str what ": expected code " code ", got " (error-brief e))))
  (when (and reason (not= reason (:reason (ex-data e))))
    (fail! (str what ": expected reason " reason ", got " (error-brief e))))
  (error-brief e))

(defn count-rows [d table]
  (q/query-value! d {:select [[[:count :*] :n]] :from table}))

;; ---------------------------------------------------------------------------
;; Schema (section 1 creates it; later sections build on it)
;; ---------------------------------------------------------------------------

(def schema
  "Three tables with foreign keys, one standalone index, one view."
  [{:create-table :orgs
    :columns [[:id :integer {:primary-key? true}]
              [:name :text {:not-null? true :unique? true}]]}
   {:create-table :users
    :columns [[:id :integer {:primary-key? true}]
              [:email :text {:not-null? true :unique? true :collate :nocase}]
              [:name :text {:not-null? true}]
              [:active :integer {:not-null? true :default true}]                       ; booleans are 0/1
              [:org-id :integer {:references {:table :orgs :column :id :on-delete :cascade}}]
              [:created-at :integer {:not-null? true                                   ; dates are epoch ms
                                     :default [:raw "(unixepoch('subsec') * 1000)"]}]]
    :indexes [[:users-org-idx [:org-id]]]}
   {:create-table :posts
    :columns [[:id :integer {:primary-key? true}]
              [:author-id :integer {:not-null? true :references {:table :users :column :id :on-delete :cascade}}]
              [:title :text {:not-null? true}]
              [:score :integer {:not-null? true :default 0}]]}
   {:create-index :posts-author-idx :on :posts :columns [:author-id [:score :desc]]}
   {:create-view :active-users :as {:select [:id :email :name] :from :users :where [:= :active 1]}}])

;; ---------------------------------------------------------------------------
;; Sections
;; ---------------------------------------------------------------------------

(defn s0-open! []
  (let [dir (mkdtemp! "sqlite-cljs-smoke-")
        path (join-path dir "db.sqlite")
        d (core/db {:path path})
        m (core/db)]
    (swap! shared assoc :dir dir :path path :file d :mem m)
    (p/let [info (core/driver-info d)
            minfo (core/driver-info m)
            sv (core/sqlite-version d)
            lv (core/library-version d)
            jm (core/pragma d :journal-mode)
            fk (core/pragma d :foreign-keys)
            sy (core/pragma d :synchronous)]
      (when-not (path-exists? path) (fail! (str "database file not created: " path)))
      (when-not (= "wal" (:journal-mode (:effective d)) jm) (fail! (str "file db journal_mode is " (pr-str jm) ", expected wal")))
      (when-not (and (:foreign-keys? (:effective d)) (= 1 fk)) (fail! "foreign_keys not on"))
      (when-not (contains? #{1 2} sy) (fail! (str "unexpected synchronous " (pr-str sy))))
      (when-not (= 5000 (:busy-timeout-ms (:effective d))) (fail! "busy_timeout default is not 5000"))
      (when-not (= "memory" (:journal-mode (:effective m))) (fail! ":memory: db should report journal_mode memory"))
      (when-not (:foreign-keys? (:effective m)) (fail! ":memory: db foreign_keys not on"))
      (when-not (and (re-matches #"3\.\d+\.\d+" sv) (= sv (:sqlite-version info))) (fail! (str "odd sqlite version " (pr-str sv))))
      (when-not (and (core/open? d) (core/open? m)) (fail! "open? false"))
      (str "file=" path
           " | driver=" (:driver info) " " lv " sqlite=" sv
           " | file effective=" (pr-str (:effective d))
           " | memory effective=" (pr-str (:effective minfo))))))

(defn s1-migrate! []
  (let [d (:file @shared)]
    (p/let [r1 (migrate/migrate! d schema {:log (fn [_] nil)})
            r2 (migrate/migrate! d schema)
            st (migrate/status! d)
            uv (core/user-version d)
            view-rows (q/query-all! d {:select [:id] :from :active-users})]
      (when-not (= [:orgs :users :users-org-idx :posts :posts-author-idx :active-users] (:created r1))
        (fail! (str "first run created " (pr-str (:created r1)))))
      (when-not (and (empty? (:created r2)) (empty? (:altered r2))
                     (= [:orgs :users :users-org-idx :posts :posts-author-idx :active-users] (:existing r2)))
        (fail! (str "re-run is not a no-op: " (pr-str (dissoc r2 :sqlite-version)))))
      (when-not (= (count schema) uv (:user-version r2)) (fail! (str "user_version " uv ", expected " (count schema))))
      (when-not (= ["orgs" "posts" "users"] (:tables st)) (fail! (str "tables " (pr-str (:tables st)))))
      (when-not (= ["active_users"] (:views st)) (fail! (str "views " (pr-str (:views st)))))
      (when-not (= ["posts_author_idx" "users_org_idx"] (:indexes st))
        (fail! (str "indexes " (pr-str (:indexes st)) " (status! hides sqlite_autoindex_* names)")))
      (when-not (= [] view-rows) (fail! "the view should be empty"))
      (str "created=" (pr-str (:created r1)) " | re-run existing=" (count (:existing r2))
           " created=" (count (:created r2)) " user-version " (:previous-user-version r1) "->" uv
           " | status tables=" (pr-str (:tables st)) " views=" (pr-str (:views st))))))

(defn s2-dsl! []
  (let [d (:file @shared)]
    (p/let [orgs (q/execute! d {:insert-into :orgs :values [{:name "acme"} {:name "globex"}] :returning [:id :name]})
            acme (:id (first (filter #(= "acme" (:name %)) orgs)))
            globex (:id (first (filter #(= "globex" (:name %)) orgs)))
            users (q/execute! d {:insert-into :users
                                 :values [{:email "ada@acme" :name "Ada" :active true :org-id acme}
                                          {:email "bob@acme" :name "Bob" :active true :org-id acme}
                                          {:email "cy@globex" :name "Cy" :active false :org-id globex}]})
            up (q/execute! d {:insert-into :users
                              :values [{:email "ADA@ACME" :name "Ada Lovelace" :active true :org-id acme}]
                              :on-conflict [:email]
                              :do-update-set {:name :excluded/name}
                              :where [:<> :users/name :excluded/name]
                              :returning [:id :name :email]})
            n (count-rows d :users)
            rows (q/query-all! d {:select [[:u/name :user] [:o/name :org] :u/email]
                                  :from [:users :u]
                                  :joins [[:join [:orgs :o] [:= :o/id :u/org-id]]]
                                  :where [:and [:= :u/active true] [:like :u/email "%@acme"]]
                                  :order-by [[:u/name :desc]]
                                  :limit 2})
            agg (q/query-all! d {:select [[:o/name :org] [[:count :*] :n]]
                                 :from [:users :u]
                                 :left-join [[:orgs :o] [:= :o/id :u/org-id]]
                                 :group-by [:o/name]
                                 :having [:> [:count :*] 0]
                                 :order-by [[:o/name :asc]]})]
      (when-not (= 2 (count orgs)) (fail! (str "orgs RETURNING " (pr-str orgs))))
      (when-not (= 3 (:changes users)) (fail! (str "users insert " (pr-str users))))
      (when-not (= [{:id 1 :name "Ada Lovelace" :email "ada@acme"}] up)
        (fail! (str "upsert RETURNING " (pr-str up) " (NOCASE collation: ADA@ACME must hit ada@acme)")))
      (when-not (= 3 n) (fail! (str "upsert inserted instead of updating: " n " users")))
      (when-not (= [{:user "Bob" :org "acme" :email "bob@acme"} {:user "Ada Lovelace" :org "acme" :email "ada@acme"}] rows)
        (fail! (str "join query rows " (pr-str rows))))
      (when-not (= [{:org "acme" :n 2} {:org "globex" :n 1}] agg) (fail! (str "group-by rows " (pr-str agg))))
      (str "orgs=" (pr-str orgs) " users.changes=" (:changes users)
           " | upsert -> " (pr-str up) " (still " n " users)"
           " | join/where/order/limit -> " (pr-str rows)
           " | group-by/having -> " (pr-str agg)))))

(defn s3-transactions! []
  (let [d (:file @shared)
        post {:insert-into :posts :values [{:author-id 1 :title "committed"}]}]
    (p/let [before (count-rows d :posts)
            committed (q/transact! d (fn [tx]
                                       (let [r @(q/execute! tx post)
                                             r2 @(q/execute! tx {:insert-into :posts :values [{:author-id 2 :title "also committed" :score 5}]})]
                                         {:rowids [(:last-insert-rowid r) (:last-insert-rowid r2)]
                                          :in-tx? (core/in-transaction? tx)})))
            after-commit (count-rows d :posts)
            [thrown] (rejection (q/transact! d (fn [tx]
                                                 @(q/execute! tx {:insert-into :posts :values [{:author-id 1 :title "doomed 1"} {:author-id 1 :title "doomed 2"}]})
                                                 (throw (ex-info "boom" {:type :smoke/boom})))))
            after-throw (count-rows d :posts)
            [deref-thrown] (rejection (q/transact! d (fn [tx]
                                                       @(q/execute! tx {:insert-into :posts :values [{:author-id 1 :title "doomed 3"}]})
                                                       @(q/execute! tx {:insert-into :posts :values [{:author-id 999 :title "fk violation"}]}))))
            after-deref (count-rows d :posts)
            nested (q/transact! d (fn [tx]
                                    @(q/execute! tx {:insert-into :posts :values [{:author-id 2 :title "outer still commits"}]})
                                    {:inner (settled-error (q/transact! tx (fn [_] :never)))
                                     :in-tx? (core/in-transaction? tx)}))
            after-nested (count-rows d :posts)
            [async-body] (rejection (q/transact! d (fn [tx] (q/execute! tx post))))
            after-async (count-rows d :posts)
            in-tx-now? (p/resolved (core/in-transaction? d))]
      (when-not (and (= 2 (- after-commit before)) (:in-tx? committed) (= 2 (count (:rowids committed))))
        (fail! (str "commit: before=" before " after=" after-commit " body=" (pr-str committed))))
      (when-not (= :smoke/boom (:type (ex-data thrown))) (fail! (str "throwing body: expected the body's error back, got " (error-brief thrown))))
      (when-not (= after-commit after-throw) (fail! (str "rollback on throw failed: " after-throw " posts, expected " after-commit)))
      (expect-error! deref-thrown "rejected deref inside the body" {:type :sqlite-cljs/sqlite-error :code "SQLITE_CONSTRAINT_FOREIGNKEY"})
      (when-not (= after-commit after-deref) (fail! "rollback on a rejected deref failed"))
      (expect-error! (:inner nested) "nested transact!" {:type :sqlite-cljs/argument-error :reason :nested-transaction})
      (when-not (and (:in-tx? nested) (= (inc after-commit) after-nested)) (fail! "the outer transaction of the nested test did not commit"))
      (expect-error! async-body "promise-returning body" {:type :sqlite-cljs/argument-error :reason :async-body})
      (when-not (= after-nested after-async) (fail! "a promise-returning body must roll back"))
      (when in-tx-now? (fail! "a transaction is still open"))
      (str "commit +2 (rowids " (pr-str (:rowids committed)) ") | throw -> rolled back (" (error-brief thrown) ")"
           " | rejected deref -> rolled back (" (util/sqlite-code deref-thrown) ")"
           " | nested -> " (error-brief (:inner nested))
           " | async body -> " (:reason (ex-data async-body))
           " | posts=" after-async))))

(defn s4-booleans-dates! []
  (let [d (:file @shared)
        t0 (- (now-ms) 86400000)
        stamp (js/Date. t0)]
    (p/let [ins (q/execute! d {:insert-into :users
                               :values [{:email "dee@globex" :name "Dee" :active false :org-id 2 :created-at (.getTime stamp)}]
                               :returning [:id]})
            id (:id (first ins))
            raw (q/query-one! d {:select [:id :active :created-at] :from :users :where [:= :id id]})
            coerced (q/query-one! d {:select [:id :active :created-at] :from :users :where [:= :id id]}
                                  {:coerce {:active :boolean :created-at :date}})
            by-false (q/query-all! d {:select [:email] :from :users :where [:= :active false]})
            by-true (q/query-value! d {:select [[[:count :*] :n]] :from :users :where [:= :active true]})
            via-view (q/query-all! d {:select [:email] :from :active-users :where [:= :email "dee@globex"]})
            defaulted (q/query-one! d {:select [:created-at] :from :users :where [:= :email "ada@acme"]} {:coerce {:created-at :date}})
            [date-dsl] (rejection (q/execute! d {:insert-into :posts :values [{:author-id 1 :title "x" :score stamp}]}))
            [date-raw] (rejection (q/execute! d ["INSERT INTO posts (author_id, title, score) VALUES (?, ?, ?)" 1 "x" stamp]))]
      (when-not (= {:id id :active 0 :created-at t0} raw) (fail! (str "raw row " (pr-str raw) " (booleans read as 0/1, dates as epoch ms)")))
      (when-not (and (false? (:active coerced)) (instance? js/Date (:created-at coerced)) (= t0 (.getTime (:created-at coerced))))
        (fail! (str "coerced row " (pr-str coerced))))
      (when-not (= [{:email "cy@globex"} {:email "dee@globex"}] (sort-by :email by-false)) (fail! (str "[:= :active false] -> " (pr-str by-false))))
      (when-not (= 2 by-true) (fail! (str "[:= :active true] count " by-true)))
      (when-not (= [] via-view) (fail! "inactive user visible through active_users"))
      (when-not (and (instance? js/Date (:created-at defaulted)) (<= (js/Math.abs (- (now-ms) (.getTime (:created-at defaulted)))) 60000))
        (fail! (str "unixepoch('subsec')*1000 default did not yield a recent epoch-ms: " (pr-str defaulted))))
      (expect-error! date-dsl "js/Date in a DSL value" {:type :sqlite-cljs/dsl-error})
      (expect-error! date-raw "js/Date as a raw parameter" {:type :sqlite-cljs/argument-error :reason :date})
      (str "wrote active=false created-at=" t0 " | raw read " (pr-str (dissoc raw :id))
           " | :coerce -> active=" (:active coerced) " created-at=" (.toISOString (:created-at coerced))
           " | where active=false -> " (mapv :email by-false) ", active=true -> " by-true
           " | DEFAULT unixepoch ms -> " (.toISOString (:created-at defaulted))
           " | js/Date rejected: dsl-error / argument-error :date"))))

(defn s5-bulk! []
  (let [d (:file @shared)
        n 1000
        rows (mapv (fn [i] {:kind (if (even? i) "even" "odd") :n i}) (range n))
        q {:insert-into :events :values rows}
        chunks (count (dsl/render-chunked q {:max-params 50}))
        t0 (now-ms)]
    (p/let [_ (q/exec! d {:create-table :events :columns [[:id :integer {:primary-key? true}] [:kind :text {:not-null? true}] [:n :integer {:not-null? true}]]})
            r (q/execute! d q {:max-params 50})
            t1 (p/resolved (now-ms))
            cnt (count-rows d :events)
            sum (q/query-value! d {:select [[[:sum :n] :s]] :from :events})
            evens (q/query-value! d {:select [[[:count :*] :c]] :from :events :where [:= :kind "even"]})
            r2 (q/execute! d {:insert-into :events :values (mapv (fn [i] {:kind "bulk2" :n i}) (range n))})
            cnt2 (count-rows d :events)
            [bad] (rejection (q/execute! d {:insert-into :events :values (conj (vec (take 99 rows)) {:kind nil :n 1})} {:max-params 50}))
            cnt3 (count-rows d :events)
            big-in (q/query-value! d {:select [[[:count :*] :c]] :from :events :where [:in :n (vec (range 0 700))]})]
      (when-not (= 40 chunks) (fail! (str "expected 40 chunks of 25 rows at :max-params 50, got " chunks)))
      (when-not (= n (:changes r)) (fail! (str "changes " (:changes r))))
      (when-not (= n cnt) (fail! (str "count " cnt)))
      (when-not (= (reduce + (range n)) sum) (fail! (str "sum " sum)))
      (when-not (= 500 evens) (fail! (str "evens " evens)))
      (when-not (and (= n (:changes r2)) (= (* 2 n) cnt2)) (fail! (str "single-statement insert: " (pr-str r2) " count " cnt2)))
      (expect-error! bad "NOT NULL violation in the last chunk" {:type :sqlite-cljs/sqlite-error :code "SQLITE_CONSTRAINT_NOTNULL"})
      (when-not (= cnt2 cnt3) (fail! (str "a failing chunk must roll back the whole insert: " cnt3)))
      (when-not (= 1400 big-in) (fail! (str "json_each IN over 700 values -> " big-in ", expected 1400")))
      (str n " rows in " chunks " chunks (:max-params 50) in " (- t1 t0) " ms, count=" cnt " sum=" sum
           " | " n " more rows in one statement (" (* 2 n) " params) -> count=" cnt2
           " | failing chunk rolled back (" (util/sqlite-code bad) ") count=" cnt3
           " | [:in :n (range 700)] via json_each -> " big-in))))

(defn s6-drift! []
  (let [d (:file @shared)
        users-def (nth schema 1)
        replace-users (fn [new-def] (assoc schema 1 new-def))
        dropped (replace-users (update users-def :columns #(vec (remove (fn [[c]] (= :name c)) %))))
        retyped (replace-users (update users-def :columns #(mapv (fn [[c t o :as col]] (if (= :name c) [c :integer o] col)) %)))
        added (replace-users (update users-def :columns conj [:nickname :text {}]))
        evolved (conj added {:alter-table :users :add-column [:nickname :text {}]})]
    (p/let [r-dropped (migrate/migrate! d dropped)
            [e-retyped] (rejection (migrate/migrate! d retyped))
            [e-added] (rejection (migrate/migrate! d added))
            uv-after-rejects (core/user-version d)
            cols-before (migrate/table-columns* d :users)
            r-evolved (migrate/migrate! d evolved)
            r-evolved-again (migrate/migrate! d evolved)
            cols-after (migrate/table-columns* d :users)
            [e-newer] (rejection (migrate/migrate! d schema))
            r-allow (migrate/migrate! d schema {:allow-newer? true})
            uv-final (core/user-version d)]
      (when-not (and (= [:users] (take 1 (drop 1 (:existing r-dropped)))) (empty? (:created r-dropped)))
        (fail! (str "a column dropped from the def is an extra DB column, which the contract tolerates; got " (pr-str (dissoc r-dropped :sqlite-version)))))
      (expect-error! e-retyped "type changed in the def" {:type :sqlite-cljs/migration-error :reason :drift})
      (when-not (= [{:column "name" :expected "INTEGER" :actual "TEXT"}] (:type-mismatch (ex-data e-retyped)))
        (fail! (str "drift data " (pr-str (select-keys (ex-data e-retyped) [:table :missing :type-mismatch])))))
      (expect-error! e-added "column added to the def" {:type :sqlite-cljs/migration-error :reason :drift})
      (when-not (and (= :users (:table (ex-data e-added))) (= ["nickname"] (:missing (ex-data e-added))))
        (fail! (str "drift data " (pr-str (select-keys (ex-data e-added) [:table :missing :type-mismatch])))))
      (when-not (= (count schema) uv-after-rejects) (fail! (str "a rejected migrate! must not touch user_version: " uv-after-rejects)))
      (when (some #(= "nickname" (:name %)) cols-before) (fail! "rejected migrate! left the nickname column behind"))
      (when-not (and (= [:users] (:altered r-evolved)) (some #(= "nickname" (:name %)) cols-after))
        (fail! (str "explicit :alter-table did not apply: " (pr-str (dissoc r-evolved :sqlite-version)))))
      (when-not (and (empty? (:altered r-evolved-again)) (empty? (:created r-evolved-again)))
        (fail! (str "evolved re-run not idempotent: " (pr-str (dissoc r-evolved-again :sqlite-version)))))
      (expect-error! e-newer "older defs vector against a newer database" {:type :sqlite-cljs/migration-error :reason :database-newer})
      (when-not (= (count schema) (:user-version r-allow) uv-final) (fail! (str ":allow-newer? run left user_version " uv-final)))
      (str "column dropped from def -> tolerated (existing) | type change -> " (error-brief e-retyped)
           " | column added -> drift missing=" (pr-str (:missing (ex-data e-added)))
           " | + explicit :alter-table :add-column -> altered=" (pr-str (:altered r-evolved)) ", re-run altered=" (pr-str (:altered r-evolved-again))
           " | fewer defs than user_version -> " (:reason (ex-data e-newer)) "; :allow-newer? true -> user_version " uv-final))))

(defn s7-injection! []
  (let [d (:file @shared)
        evil "Robert'); DROP TABLE users;--"
        evil-ident "users\"; DROP TABLE users;--"]
    (p/let [before (count-rows d :users)
            ins (q/execute! d {:insert-into :users :values [{:email "bobby@tables" :name evil :active true :org-id 1}] :returning [:id :name]})
            back (q/query-one! d {:select [:name :email] :from :users :where [:= :name evil]})
            like (q/query-all! d {:select [:email] :from :users :where [:like :name "%DROP TABLE%"]})
            raw-back (q/query-one! d ["SELECT name FROM users WHERE name = ?" evil])
            rendered (p/resolved (dsl/render {:select [:id] :from evil-ident}))
            [ident-err] (rejection (q/query-all! d {:select [:id] :from evil-ident}))
            after (count-rows d :users)
            st (migrate/status! d)
            upd (q/execute! d {:update :users :set {:name [:|| :name "'; DROP TABLE posts; --"]} :where [:= :email "bobby@tables"] :returning [:name]})
            posts (count-rows d :posts)]
      (when-not (= evil (:name (first ins)) (:name back) (:name raw-back)) (fail! (str "value not stored verbatim: " (pr-str [ins back raw-back]))))
      (when-not (= [{:email "bobby@tables"}] like) (fail! (str "LIKE lookup " (pr-str like))))
      (when-not (= "SELECT \"id\" FROM \"users\"\"; DROP TABLE users;--\"" (:sql rendered)) (fail! (str "identifier not quoted inertly: " (:sql rendered))))
      (expect-error! ident-err "hostile identifier" {:type :sqlite-cljs/sqlite-error :code "SQLITE_ERROR"})
      (when-not (= (inc before) after) (fail! (str "users count " after)))
      (when-not (= ["events" "orgs" "posts" "users"] (:tables st)) (fail! (str "tables after the attempts: " (pr-str (:tables st)))))
      (when-not (= (str evil "'; DROP TABLE posts; --") (:name (first upd))) (fail! (str "update RETURNING " (pr-str upd))))
      (when-not (pos? posts) (fail! "posts table gone"))
      (str "stored+read verbatim " (pr-str evil) " (DSL and raw ?) | hostile :from rendered as " (pr-str (:sql rendered))
           " -> " (error-brief ident-err) " | tables intact: " (pr-str (:tables st)) " posts=" posts))))

(defn s8-cache! []
  (let [d (:file @shared)
        q {:select [:id :email] :from :users :where [:= :id 1]}
        sql (:sql (q/->statement q))
        t0 (now-ms)]
    (p/let [_ (q/clear-cache! d)
            size0 (p/resolved (core/cache-size d))
            _ (p/loop [i 0]
                (when (< i 200)
                  (p/let [row (q/query-one! d q)]
                    (when-not (= 1 (:id row)) (fail! (str "run " i " returned " (pr-str row))))
                    (p/recur (inc i)))))
            t1 (p/resolved (now-ms))
            size1 (p/resolved (core/cache-size d))
            hit? (p/resolved (core/cached? d sql))
            _ (p/loop [i 0]
                (when (< i 140)
                  ;; distinct SQL TEXT per iteration (a bound parameter would not change the cache key)
                  (p/let [_ (q/query-value! d {:select [[[:raw (str i)] :v]] :from :users :where [:= :id 1]})]
                    (p/recur (inc i)))))
            size2 (p/resolved (core/cache-size d))
            evicted? (p/resolved (not (core/cached? d sql)))
            _ (q/exec! d "SELECT 1")
            size3 (p/resolved (core/cache-size d))]
      (when-not (= 0 size0) (fail! (str "clear-cache! left " size0)))
      (when-not (and (= 1 size1) hit?) (fail! (str "200 runs of one query: cache-size=" size1 " cached?=" hit?)))
      (when-not (and (= core/default-cache-size size2) evicted?) (fail! (str "LRU: size=" size2 " after 140 distinct statements, original evicted?=" evicted?)))
      (when-not (= 0 size3) (fail! (str "exec! must clear the cache: " size3)))
      (str "200 x query-one! in " (- t1 t0) " ms -> cache-size=" size1 " cached?=" hit?
           " | 140 distinct statements -> size=" size2 " (LRU cap " core/default-cache-size "), first query evicted=" evicted?
           " | exec! -> size=" size3))))

(defn s9-strict! []
  (let [d (:file @shared)
        m (:mem @shared)
        try-render (fn [def] (try (ddl/render def) nil (catch :default e e)))
        bool-err (try-render {:create-table :flags :columns [[:id :integer {:primary-key? true}] [:on :boolean {}]]})
        date-err (try-render {:create-table :flags :columns [[:id :integer {:primary-key? true}] [:at :datetime {}]]})
        raw-strict-err (try-render {:create-table :legacy :columns [[:id :integer {:primary-key? true}] [:label [:raw "VARCHAR(255)"] {}]]})
        numeric-err (try-render {:create-table :legacy :columns [[:id :integer {:primary-key? true}] [:amount :numeric {}]]})
        legacy {:create-table :legacy :strict? false
                :columns [[:id :integer {:primary-key? true}] [:label [:raw "VARCHAR(255)"] {}] [:amount :numeric {}]]}
        legacy-sql (first (:statements (ddl/render legacy)))]
    (p/let [_ (q/exec! m {:create-table :flags :columns [[:id :integer {:primary-key? true}] [:on :integer {:not-null? true :check [:in :on [0 1]]}]]})
            ok (q/execute! m {:insert-into :flags :values [{:on true}]})
            [bad-type] (rejection (q/execute! m {:insert-into :flags :values [{:on "yes"}]}))
            [bad-check] (rejection (q/execute! m {:insert-into :flags :values [{:on 2}]}))
            _ (q/exec! d legacy)
            leg (q/execute! d {:insert-into :legacy :values [{:label "x" :amount 1.5}]})
            leg-row (q/query-one! d {:select [:label :amount] :from :legacy})
            leg-sql (q/query-value! d ["SELECT sql FROM sqlite_master WHERE name = 'legacy'"])
            leg-cols (migrate/table-columns* d :legacy)
            info (migrate/migrate! d (conj schema {:alter-table :users :add-column [:nickname :text {}]} legacy) {:allow-newer? true})]
      (expect-error! bool-err ":boolean column" {:type :sqlite-cljs/argument-error :reason :unknown-type})
      (when-not (= :on (:column (ex-data bool-err))) (fail! (str ":boolean error names the wrong column: " (pr-str (ex-data bool-err)))))
      (expect-error! date-err ":datetime column" {:type :sqlite-cljs/argument-error :reason :unknown-type})
      (expect-error! raw-strict-err "[:raw VARCHAR] in a STRICT table" {:type :sqlite-cljs/argument-error :reason :strict-type})
      (expect-error! numeric-err ":numeric in a STRICT table" {:type :sqlite-cljs/argument-error :reason :strict-type})
      (when-not (= 1 (:changes ok)) (fail! ":integer boolean column insert failed"))
      (expect-error! bad-type "text into a STRICT INTEGER column" {:type :sqlite-cljs/sqlite-error :code "SQLITE_CONSTRAINT_DATATYPE"})
      (expect-error! bad-check "2 into a 0/1 CHECK column" {:type :sqlite-cljs/sqlite-error :code "SQLITE_CONSTRAINT_CHECK"})
      (when-not (str/ends-with? legacy-sql ")") (fail! (str "legacy table must not be STRICT: " legacy-sql)))
      (when-not (str/includes? legacy-sql "\"label\" VARCHAR(255)") (fail! (str "legacy SQL " legacy-sql)))
      (when-not (and (= 1 (:changes leg)) (= {:label "x" :amount 1.5} leg-row)) (fail! (str "legacy insert/select " (pr-str [leg leg-row]))))
      (when-not (= "VARCHAR(255)" (:type (first (filter #(= "label" (:name %)) leg-cols)))) (fail! (str "table_xinfo type " (pr-str leg-cols))))
      (when-not (= [:legacy] (filter #{:legacy} (:existing info))) (fail! (str "migrate! must verify the legacy table clean (raw type compared verbatim): " (pr-str (dissoc info :sqlite-version)))))
      (str ":boolean -> " (error-brief bool-err) " | :datetime -> " (:reason (ex-data date-err))
           " | [:raw VARCHAR] under STRICT -> " (:reason (ex-data raw-strict-err))
           " | :integer 0/1 + CHECK works; 'yes' -> " (util/sqlite-code bad-type) ", 2 -> " (util/sqlite-code bad-check)
           " | :strict? false legacy: " (pr-str legacy-sql) " insert/select " (pr-str leg-row) ", xinfo type " (pr-str (:type (first (filter #(= "label" (:name %)) leg-cols))))
           " | migrate! verifies it clean (existing)"))))

(defn s10-errors! []
  (let [d (:file @shared)
        m (:mem @shared)
        cfg (fn [thunk] (try (thunk) nil (catch :default e e)))
        cfg-err (cfg #(core/db {:memory? true :readonly? true}))
        dsl-err (cfg #(dsl/render {:select [:id] :from :t :where [:= :id]}))
        where-less (cfg #(dsl/render {:delete-from :t}))
        nan-err (cfg #(core/->param js/NaN))
        inf-err (cfg #(core/->param js/Infinity))]
    (p/let [[uniq] (rejection (q/execute! d {:insert-into :orgs :values [{:name "acme"}]}))
            [fk] (rejection (q/execute! d {:insert-into :posts :values [{:author-id 424242 :title "orphan"}]}))
            [notnull] (rejection (q/execute! d {:insert-into :orgs :values [{:name nil}]}))
            [datatype] (rejection (q/execute! d ["INSERT INTO posts (author_id, title, score) VALUES (?, ?, ?)" 1 "t" "high"]))
            [coll] (rejection (q/execute! d ["INSERT INTO orgs (name) VALUES (?)" [1 2]]))
            [count-err] (rejection (q/query-all! d ["SELECT ? AS a" 1 2]))
            [multi] (rejection (q/query-all! d "SELECT 1; SELECT 2"))
            [syntax] (rejection (q/query-all! d "SELEKT 1"))
            [no-table] (rejection (q/query-all! d {:select [:id] :from :nope}))
            [cast-err] (rejection (q/query-all! d {:select [[[:cast :id :varchar] :v]] :from :users}))
            _ (core/close! m)
            [closed] (rejection (q/query-all! m "SELECT 1 AS v"))
            ro (p/resolved (core/db {:path (:path @shared) :readonly? true :file-must-exist? true}))
            [ro-write] (rejection (q/execute! ro {:insert-into :orgs :values [{:name "initech"}]}))
            ro-read (q/query-value! ro {:select [[[:count :*] :n]] :from :orgs})
            _ (core/close! ro)
            [cantopen] (p/resolved [(cfg #(core/db {:path (join-path (:dir @shared) "missing.sqlite") :file-must-exist? true}))])]
      (expect-error! uniq "UNIQUE violation" {:type :sqlite-cljs/sqlite-error :code "SQLITE_CONSTRAINT_UNIQUE"})
      (when-not (and (util/constraint-error? uniq) (not (util/busy-error? uniq)) (string? (:sql (ex-data uniq)))) (fail! "constraint-error?/:sql on the unique error"))
      (expect-error! fk "FK violation" {:type :sqlite-cljs/sqlite-error :code "SQLITE_CONSTRAINT_FOREIGNKEY"})
      (expect-error! notnull "NOT NULL violation" {:type :sqlite-cljs/sqlite-error :code "SQLITE_CONSTRAINT_NOTNULL"})
      (expect-error! datatype "STRICT datatype violation" {:type :sqlite-cljs/sqlite-error :code "SQLITE_CONSTRAINT_DATATYPE"})
      (expect-error! coll "collection as a bind value" {:type :sqlite-cljs/argument-error :reason :collection})
      (expect-error! count-err "parameter count mismatch" {:type :sqlite-cljs/argument-error :reason :parameter-count})
      (expect-error! multi "two statements in one string" {:type :sqlite-cljs/argument-error :reason :multiple-statements})
      (expect-error! syntax "syntax error" {:type :sqlite-cljs/sqlite-error :code "SQLITE_ERROR"})
      (expect-error! no-table "no such table" {:type :sqlite-cljs/sqlite-error :code "SQLITE_ERROR"})
      (expect-error! cast-err "unknown CAST type" {:type :sqlite-cljs/dsl-error})
      (expect-error! closed "closed connection" {:type :sqlite-cljs/argument-error :reason :closed})
      (expect-error! ro-write "write on a read-only connection" {:type :sqlite-cljs/sqlite-error :code "SQLITE_READONLY"})
      (when-not (= 2 ro-read) (fail! (str "read-only connection count " ro-read)))
      (expect-error! cantopen ":file-must-exist? on a missing file" {:type :sqlite-cljs/sqlite-error :code "SQLITE_CANTOPEN"})
      (expect-error! cfg-err "readonly :memory:" {:type :sqlite-cljs/config-error :reason :bad-options})
      (expect-error! dsl-err "malformed [:= x]" {:type :sqlite-cljs/dsl-error})
      (when-not (vector? (:form (ex-data dsl-err))) (fail! "dsl-error must carry :form"))
      (expect-error! where-less "WHERE-less delete" {:type :sqlite-cljs/dsl-error})
      (expect-error! nan-err "NaN" {:type :sqlite-cljs/argument-error :reason :nan})
      (expect-error! inf-err "Infinity" {:type :sqlite-cljs/argument-error :reason :infinity})
      (str "sqlite-error codes: " (str/join ", " (map util/sqlite-code [uniq fk notnull datatype syntax ro-write cantopen]))
           " | argument-error reasons: " (str/join ", " (map #(name (:reason (ex-data %))) [coll count-err multi closed nan-err inf-err]))
           " | dsl-error x3 (with :form), config-error " (:reason (ex-data cfg-err))
           " | constraint-error? true, busy-error? false on the UNIQUE error"))))

;; ---------------------------------------------------------------------------
;; Main
;; ---------------------------------------------------------------------------

(defn- cleanup! []
  (let [{:keys [file mem dir]} @shared]
    (-> (p/do
          (when file (core/close! file {:optimize? true}))
          (when mem (core/close! mem)))
        (p/catch (fn [e] (say "cleanup: close failed:" (error-brief e))))
        (p/then (fn [_]
                  (when dir
                    (rm-rf! dir)
                    (say (str "cleanup: removed " dir " (gone=" (not (path-exists? dir)) ")"))))))))

(defn main [& _]
  (say "sqlite-cljs LIVE smoke —" (js/Date.) "— file database in a temp dir + :memory:, no network")
  (-> (p/do
        (section! "0 open file + :memory: db (versions, effective pragmas)" s0-open!)
        (section! "1 migrate! schema (3 tables/FK, index, view) + idempotent re-run" s1-migrate!)
        (section! "2 DSL multi-row insert, upsert RETURNING, join/where/order/limit" s2-dsl!)
        (section! "3 transactions: commit, rollback on throw, nested rejected" s3-transactions!)
        (section! "4 booleans 0/1 + epoch-ms dates round trip (:coerce)" s4-booleans-dates!)
        (section! "5 bulk insert 1000 rows, chunking path (:max-params 50)" s5-bulk!)
        (section! "6 drift -> migration-error, explicit alter, user_version guard" s6-drift!)
        (section! "7 injection safety live (value + identifier positions)" s7-injection!)
        (section! "8 statement cache (200 runs, LRU cap, exec! clears)" s8-cache!)
        (section! "9 STRICT mode (:boolean rejected, :integer works, legacy :raw + :strict? false)" s9-strict!)
        (section! "10 error taxonomy (SQLITE_* codes, argument-error reasons)" s10-errors!))
      (p/catch (fn [e] (say "FAIL harness:" (error-brief e))))
      (p/then (fn [_] (cleanup!)))
      (p/finally (fn [_ _]
                   (let [rs @results
                         failed (remove :ok? rs)]
                     (say "")
                     (say (str "TALLY: " (count (filter :ok? rs)) "/" (count rs) " sections passed"))
                     (doseq [f failed] (say (str "  FAILED " (:name f) ": " (:reason f))))
                     (js/process.exit (if (and (seq rs) (empty? failed)) 0 1)))))))
