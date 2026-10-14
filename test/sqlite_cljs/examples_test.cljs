(ns sqlite-cljs.examples-test
  "The examples of the README, run. Network-free: the databases are in
   memory, and one is a file in a fresh temp directory that is removed
   afterwards.

   The README is held to the code:
   - what stands between a README:begin and a README:end mark of
     dev/sqlite_cljs/examples.cljs stands in README.md as written;
   - every line the quick start prints stands in README.md;
   - every one-liner of `shown` stands in README.md with the value it
     evaluates to;
   - the results README.md shows for the other examples are the results
     they resolve to here.
   Spacing and commas are not compared."
  (:require ["fs" :as fs]
            ["os" :as os]
            ["path" :as path]
            [cljs.test :refer [is testing]]
            [clojure.string :as str]
            [promesa.core :as p]
            [sqlite-cljs.core :as sqlite]
            [sqlite-cljs.ddl :as ddl]
            [sqlite-cljs.dsl :as dsl]
            [sqlite-cljs.examples :as examples]
            [sqlite-cljs.migrate :as migrate]
            [sqlite-cljs.query :as q]
            [sqlite-cljs.test-util :as tu]
            [sqlite-cljs.util :as util])
  (:require-macros [sqlite-cljs.doctest :refer [claims]]
                   [sqlite-cljs.test-util :refer [deftest-p]]))

;; ---------------------------------------------------------------------------
;; The one-liners
;; ---------------------------------------------------------------------------

(defn shown
  "The one-liners of the README, each with its value. `db` is an open
   database in memory."
  [db]
  (claims
   (dsl/render {:select [:id] :from :users :where [:= :name "x'; DROP TABLE users; --"]})
   (dsl/render {:select [:u/name [[:count :*] :orders]] :from [[:users :u]] :left-join [[:orders :o] [:= :o/user-id :u/id]] :group-by [:u/name]})
   (dsl/render {:select [:*] :from :users :where [:in :id [1 2 3]]})
   (dsl/render {:select [:*] :from :users :where [:= :country nil]})
   (dsl/render {:delete-from :users :where [:< :score 0]})
   (count (:params (dsl/render {:select [:*] :from :users :where [:in :id (range 1000)]})))
   (:statements (ddl/render {:create-table :tags :columns [[:id :integer {:primary-key? true}] [:label :text {:not-null? true}]]}))
   (:statements (ddl/render {:create-index :tags-label-idx :on :tags :columns [:label] :unique? true}))
   (util/sql-ident :user-name)
   (util/sql-ident "we\"ird")
   (:effective db)
   (:memory? db)
   (sqlite/open? db)))

;; ---------------------------------------------------------------------------
;; What the README states the examples resolve to
;; ---------------------------------------------------------------------------

(def first-run-shown
  "{:created [:users :users-score-idx]
    :existing []
    :altered []
    :user-version 1
    :previous-user-version 0}")

(def upgrade-shown
  "{:created [:orders :orders-user-idx]
    :existing [:users :users-score-idx]
    :altered [:users]
    :user-version 3
    :previous-user-version 1}")

(def again-shown
  "{:created []
    :existing [:users :users-score-idx :users :orders :orders-user-idx]
    :altered []
    :user-version 3
    :previous-user-version 3}")

(def drift-shown
  "{:type :sqlite-cljs/migration-error
    :reason :drift
    :table :users
    :missing [\"country\"]
    :type-mismatch []}")

(def newer-shown
  "{:type :sqlite-cljs/migration-error
    :reason :database-newer
    :user-version 3
    :defs 1}")

(def transfer-shown
  "{\"ada@example.com\" 520 \"bob@example.com\" 115}")

(def refused-shown
  "{:message \"not enough points\"
    :data {:from \"bob@example.com\" :points 500}
    :scores [{:email \"ada@example.com\" :score 520} {:email \"bob@example.com\" :score 115}]}")

(def saved-shown
  "{:id 5 :email \"eve@example.com\" :name \"Eve\" :score 1}")

(def saved-again-shown
  "{:id 5 :email \"eve@example.com\" :name \"Eve Adams\" :score 40}")

(def added-shown
  "{:id 5}")

(def duplicate-shown
  "{:failed {:type :sqlite-cljs/sqlite-error
             :code \"SQLITE_CONSTRAINT_UNIQUE\"
             :message \"UNIQUE constraint failed: users.email\"}}")

(def orphan-shown
  "{:type :sqlite-cljs/sqlite-error
    :code \"SQLITE_CONSTRAINT_FOREIGNKEY\"
    :message \"FOREIGN KEY constraint failed\"}")

(def strict-shown
  "{:type :sqlite-cljs/sqlite-error
    :code \"SQLITE_CONSTRAINT_DATATYPE\"
    :message \"cannot store TEXT value in INTEGER column users.score\"}")

(def dsl-shown
  "{:type :sqlite-cljs/dsl-error :form {:update :users :set {:score 0}}}")

;; ---------------------------------------------------------------------------
;; Reading the README
;; ---------------------------------------------------------------------------

(defn- slurp [file]
  (fs/readFileSync file "utf8"))

(defn- plain
  "`text` without what is not compared: commas, and any run of whitespace
   as one space."
  [text]
  (-> text
      (str/replace "," "")
      (str/replace #"\s+" " ")
      str/trim))

(defn- region
  "The text between the marks README:begin `label` and README:end `label`
   of the file at `file`, nil when either mark is missing."
  [file label]
  (let [[_ found] (re-find (re-pattern (str "(?s);; README:begin " label "\\n(.*?)\\n\\s*;; README:end " label))
                           (slurp file))]
    found))

(defn- printed
  "`x` as the README prints it."
  [x]
  (plain (pr-str x)))

(defn- ordered
  "`m` with the keys `ks` in that order (the order the README prints)."
  [m ks]
  (apply array-map (mapcat (fn [k] [k (get m k)]) (filter #(contains? m %) ks))))

(def ^:private report-keys
  [:created :existing :altered :user-version :previous-user-version])

(def ^:private error-keys
  [:type :code :reason :table :missing :type-mismatch :user-version :defs :message :form])

(defn- temp-dir! []
  (fs/mkdtempSync (path/join (os/tmpdir) "sqlite-cljs-examples-")))

(defn- seeded!
  "Promise of a database in memory after the quick start."
  []
  (let [db (sqlite/db {:memory? true})]
    (p/let [_ (examples/quick-start db)]
      db)))

(defn- scores! [db emails]
  (q/query-all! db {:select [:email :score] :from :users
                    :where [:in :email emails]
                    :order-by [:email]}))

;; ---------------------------------------------------------------------------
;; Tests
;; ---------------------------------------------------------------------------

(deftest-p the-quick-start-reads-what-it-says
  (let [db (sqlite/db {:memory? true})]
    (p/let [{:keys [created inserted sql rows one total updated] :as read} (examples/quick-start db)
            _ (sqlite/close! db)]
      (testing "every printed key is read, and no other"
        (is (= (set examples/printed) (set (keys read)))))
      (testing "the table and its index were created"
        (is (= [:users :users-score-idx] created)))
      (testing "four rows went in with one statement"
        (is (= {:changes 4 :last-insert-rowid 4} inserted)))
      (testing "the query is SQL with placeholders; the values travel beside it"
        (is (= [true 50] (:params sql)))
        (is (= 2 (count (re-seq #"\?" (:sql sql)))))
        (is (not (str/includes? (:sql sql) "50"))))
      (testing "rows are kebab-case maps, in the order asked for"
        (is (= [{:id 1 :name "Ada" :score 620 :active true}
                {:id 3 :name "Carol" :score 75 :active true}]
               rows)))
      (testing "the email column compares without case"
        (is (= {:id 2 :email "bob@example.com"} one)))
      (testing "one value, and the rows an update returns"
        (is (= 4 total))
        (is (= [{:name "Bob" :score 15}] updated)))
      (testing "the database is closed"
        (is (not (sqlite/open? db)))))))

(deftest-p the-quick-start-on-a-file
  (let [dir (temp-dir!)
        file (path/join dir "app.db")
        db (sqlite/db file)]
    (-> (p/let [read (examples/quick-start db)
                _ (sqlite/close! db)
                again (sqlite/db {:path file :readonly? true})
                total (q/query-value! again {:select [[[:count :*] :n]] :from :users})
                _ (sqlite/close! again)]
          (is (= 4 (:total read) total) "the rows are in the file")
          (is (= "wal" (get-in db [:effective :journal-mode])))
          (is (fs/existsSync file)))
        (p/finally (fn [_ _] (fs/rmSync dir #js {:recursive true :force true}))))))

(deftest-p a-schema-that-grows
  (let [db (sqlite/db {:memory? true})]
    (p/let [first-run (examples/upgrade! db examples/schema)
            _ (q/execute! db {:insert-into :users :values [{:email "ada@example.com" :name "Ada"}]})
            upgraded (examples/upgrade! db examples/schema-2)
            again (examples/upgrade! db examples/schema-2)
            row (q/query-one! db {:select [:name :country] :from :users})
            status (migrate/status! db)
            [newer] (tu/rejection (migrate/migrate! db examples/schema))
            fresh (sqlite/db {:memory? true})
            on-fresh (examples/upgrade! fresh examples/schema-2)
            _ (sqlite/close! fresh)
            old (sqlite/db {:memory? true})
            _ (examples/upgrade! old examples/schema)
            [missing] (tu/rejection (migrate/migrate! old [(first examples/schema-2)]))
            after (migrate/status! old)
            _ (sqlite/close! old)
            _ (sqlite/close! db)]
      (testing "the first run creates"
        (is (= (plain first-run-shown) (printed (ordered first-run report-keys)))))
      (testing "the upgrade adds the column and the table, and keeps the rows"
        (is (= (plain upgrade-shown) (printed (ordered upgraded report-keys))))
        (is (= {:name "Ada" :country nil} row))
        (is (= ["orders" "users"] (:tables status))))
      (testing "a second run does nothing"
        (is (= (plain again-shown) (printed (ordered again report-keys)))))
      (testing "an empty database gets the new shape at once, and skips the alter"
        (is (= [:users :users-score-idx :orders :orders-user-idx] (:created on-fresh)))
        (is (= [] (:altered on-fresh))))
      (testing "definitions older than the database are refused"
        (is (= (plain newer-shown) (printed (ordered (util/error-data newer) error-keys)))))
      (testing "a column declared without its alter is drift, and nothing is applied"
        (is (= (plain drift-shown) (printed (ordered (util/error-data missing) error-keys))))
        (is (= 1 (:user-version after)))))))

(deftest-p all-or-nothing
  (p/let [db (seeded!)
          moved (examples/transfer! db "ada@example.com" "bob@example.com" 100)
          [refused] (tu/rejection (examples/transfer! db "bob@example.com" "ada@example.com" 500))
          tried (examples/try-transfer! db "bob@example.com" "ada@example.com" 500)
          scores (scores! db ["ada@example.com" "bob@example.com"])
          _ (sqlite/close! db)]
    (testing "both updates are applied"
      (is (= {"ada@example.com" 520 "bob@example.com" 115} moved))
      (is (= (plain transfer-shown) (printed (ordered moved ["ada@example.com" "bob@example.com"])))))
    (testing "a throw in the body undoes both"
      (is (= "not enough points" (ex-message refused)))
      (is (= {:from "bob@example.com" :points 500} (ex-data refused)))
      (is (= [{:email "ada@example.com" :score 520} {:email "bob@example.com" :score 115}] scores))
      (is (= {:message "not enough points"
              :data {:from "bob@example.com" :points 500}
              :scores scores}
             tried))
      (is (= (plain refused-shown)
             (printed (array-map :message (:message tried)
                                 :data (ordered (:data tried) [:from :points])
                                 :scores (mapv #(ordered % [:email :score]) (:scores tried)))))))
    (testing "no transaction is left open"
      (is (not (sqlite/in-transaction? db))))))

(deftest-p insert-or-update
  (p/let [db (seeded!)
          saved (examples/save-user! db {:email "eve@example.com" :name "Eve" :score 1})
          again (examples/save-user! db {:email "EVE@example.com" :name "Eve Adams" :score 40})
          total (q/query-value! db {:select [[[:count :*] :n]] :from :users})
          _ (sqlite/close! db)]
    (is (= (plain saved-shown) (printed (ordered saved [:id :email :name :score]))))
    (is (= (plain saved-again-shown) (printed (ordered again [:id :email :name :score]))))
    (is (= 5 total) "one row, not two")))

(deftest-p a-failure-is-data
  (p/let [db (seeded!)
          _ (examples/upgrade! db examples/schema-2)
          added (examples/add-user! db {:email "eve@example.com" :name "Eve"})
          duplicate (examples/add-user! db {:email "ADA@example.com" :name "Ada again"})
          [orphan] (tu/rejection (q/execute! db {:insert-into :orders
                                                 :values [{:user-id 99 :amount 5 :placed-at 0}]}))
          [strict] (tu/rejection (q/execute! db {:update :users :set {:score "many"}
                                                 :where [:= :name "Ada"]}))
          [dsl] (tu/rejection (q/execute! db {:update :users :set {:score 0}}))
          total (q/query-value! db {:select [[[:count :*] :n]] :from :users})
          _ (sqlite/close! db)]
    (testing "an insert resolves"
      (is (= (plain added-shown) (printed added))))
    (testing "a second user with the email is refused by the database"
      (is (= (plain duplicate-shown)
             (printed (update duplicate :failed ordered error-keys))))
      (is (= 5 total)))
    (testing "foreign keys are enforced"
      (is (util/constraint-error? orphan))
      (is (= (plain orphan-shown) (printed (ordered (examples/explain orphan) error-keys)))))
    (testing "a STRICT table refuses a value of another type"
      (is (= (plain strict-shown) (printed (ordered (examples/explain strict) error-keys)))))
    (testing "an update without :where is refused before any SQL runs"
      (is (= (plain dsl-shown)
             (printed (ordered (util/error-data dsl) error-keys)))))))

(deftest-p the-one-liners-hold
  (let [db (sqlite/db {:memory? true})
        claimed (shown db)
        value (into {} claimed)]
    (p/let [_ (sqlite/close! db)]
      (is (= (count claimed) (count value)) "no one-liner is shown twice")
      (is (= ["x'; DROP TABLE users; --"]
             (:params (value "(dsl/render {:select [:id], :from :users, :where [:= :name \"x'; DROP TABLE users; --\"]})"))))
      (is (= 1 (value "(count (:params (dsl/render {:select [:*], :from :users, :where [:in :id (range 1000)]})))")))
      (is (= "\"user_name\"" (value "(util/sql-ident :user-name)")))
      (is (= "\"we\"\"ird\"" (value "(util/sql-ident \"we\\\"ird\")")))
      (is (true? (:foreign-keys? (value "(:effective db)"))))
      (is (= 5000 (:busy-timeout-ms (value "(:effective db)"))))
      (is (every? (comp some? second) claimed)))))

(deftest-p the-readme-shows-what-runs
  (let [db (sqlite/db {:memory? true})
        readme (plain (slurp "README.md"))
        shows? (fn [text] (str/includes? readme (plain text)))]
    (p/let [read (examples/quick-start db)
            claimed (shown db)
            _ (sqlite/close! db)]
      (testing "the marked code of the examples"
        (doseq [label ["quick-start" "schema" "transfer" "upsert" "errors"]
                :let [code (region "dev/sqlite_cljs/examples.cljs" label)]]
          (is (some? code) (str "no region " label))
          (is (shows? code) (str "README.md does not show " label ":\n" code))))
      (testing "every line the quick start prints"
        (doseq [k examples/printed
                :let [line (str k " " (pr-str (get read k)))]]
          (is (shows? line) (str "README.md does not show the line:\n" line))))
      (testing "every one-liner, with its value"
        (doseq [[source value] claimed
                :let [line (str source "\n;; => " (pr-str value))]]
          (is (shows? line) (str "README.md does not show:\n" line))))
      (testing "what the examples resolve to"
        (doseq [text [first-run-shown upgrade-shown again-shown drift-shown newer-shown
                      transfer-shown
                      refused-shown saved-shown saved-again-shown added-shown
                      duplicate-shown orphan-shown strict-shown dsl-shown]]
          (is (shows? text) (str "README.md does not show:\n" text))))
      (testing "the pointers it gives"
        (is (shows? "docs/sqlite-cljs.org"))
        (is (shows? "docs/sqlite-api-inventory.org"))
        (is (fs/existsSync "docs/sqlite-cljs.org"))
        (is (fs/existsSync "docs/sqlite-api-inventory.org"))))))
