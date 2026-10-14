(ns sqlite-cljs.ddl-test
  "DDL renderer tests: golden SQL strings and validation, no database."
  (:require [cljs.test :refer [deftest is testing]]
            [clojure.string :as str]
            [sqlite-cljs.ddl :as ddl]))

(defn- err [thunk]
  (try (thunk) nil (catch :default e (ex-data e))))

(defn- arg-error? [thunk]
  (= :sqlite-cljs/argument-error (:type (err thunk))))

(defn- one [d] (first (:statements (ddl/render d))))

(deftest create-table-golden
  (is (= {:statements ["CREATE TABLE IF NOT EXISTS \"users\" (\"id\" INTEGER PRIMARY KEY, \"email\" TEXT NOT NULL UNIQUE COLLATE NOCASE, \"name\" TEXT NOT NULL, \"age\" INTEGER CHECK (\"age\" >= 0), \"active\" INTEGER NOT NULL DEFAULT 1, \"org_id\" INTEGER REFERENCES \"orgs\"(\"id\") ON DELETE CASCADE ON UPDATE RESTRICT DEFERRABLE INITIALLY DEFERRED, \"created_at\" INTEGER NOT NULL DEFAULT (unixepoch('subsec') * 1000), \"updated_at\" TEXT DEFAULT CURRENT_TIMESTAMP, \"data\" TEXT DEFAULT '{}', \"label\" TEXT GENERATED ALWAYS AS ((\"name\" || ' <' || \"email\" || '>')) STORED, \"legacy\" ANY) STRICT"
                       "CREATE UNIQUE INDEX IF NOT EXISTS \"users_email_idx\" ON \"users\" (\"email\")"
                       "CREATE INDEX IF NOT EXISTS \"users_org_id_created_at_idx\" ON \"users\" (\"org_id\", \"created_at\" DESC) WHERE \"active\" = 1"]}
         (ddl/render-table-def
          {:create-table :users
           :columns [[:id :integer {:primary-key? true}]
                     [:email :text {:not-null? true :unique? true :collate :nocase}]
                     [:name :text {:not-null? true}]
                     [:age :integer {:check [:>= :age 0]}]
                     [:active :integer {:not-null? true :default true}]
                     [:org-id :integer {:references {:table :orgs :column :id :on-delete :cascade :on-update :restrict
                                                     :deferrable :initially-deferred}}]
                     [:created-at :integer {:not-null? true :default [:raw "(unixepoch('subsec') * 1000)"]}]
                     [:updated-at :text {:default :current-timestamp}]
                     [:data :text {:default "{}"}]
                     [:label :text {:generated-as {:expr [:|| :name " <" :email ">"] :stored? true}}]
                     [:legacy :any {}]]
           :indexes [[:users-email-idx [:email] {:unique? true}]
                     [nil [:org-id [:created-at :desc]] {:where [:= :active 1]}]]}))
      "inventory §6.3.1 example (task spelling of the option keys)"))

(deftest create-table-inventory-spellings-accepted
  (is (= (one {:create-table :t :columns [[:id :integer {:primary-key? true}]
                                          [:org-id :integer {:references {:table :orgs :column :id :on-delete :cascade}}]
                                          [:label :text {:generated-as {:expr [:upper :name] :stored? false}}]]})
         (one {:create-table :t :columns [[:id :integer {:primary-key true}]
                                          [:org-id :integer {:references [:orgs :id] :on-delete :cascade}]
                                          [:label :text {:generated [:upper :name] :stored false}]]}))
      ":primary-key / :references [t c] / :generated aliases render identically")
  (is (= "CREATE TABLE IF NOT EXISTS \"t\" (\"id\" INTEGER PRIMARY KEY, \"label\" TEXT GENERATED ALWAYS AS (upper(\"name\")) VIRTUAL, \"name\" TEXT) STRICT"
         (one {:create-table :t :columns [[:id :integer {:primary-key true}]
                                          [:label :text {:generated [:upper :name]}]
                                          [:name :text {}]]}))))

(deftest table-options-and-constraints
  (is (= "CREATE TABLE \"kv\" (\"k\" TEXT NOT NULL, \"v\" BLOB, PRIMARY KEY (\"k\")) WITHOUT ROWID, STRICT"
         (one {:create-table :kv :if-not-exists? false :without-rowid? true
               :columns [[:k :text {:not-null? true}] [:v :blob {}]]
               :primary-key [:k]})))
  (is (= "CREATE TEMP TABLE IF NOT EXISTS \"tmp\" (\"a\" INTEGER, \"b\" NUMERIC, \"c\" VARCHAR(255), \"d\")"
         (one {:create-table :tmp :temp? true :strict? false
               :columns [[:a :integer] [:b :numeric {}] [:c [:raw "VARCHAR(255)"] {}] [:d nil {}]]}))
      "non-strict: NUMERIC, raw legacy types and untyped columns")
  (is (= "CREATE TABLE IF NOT EXISTS \"m\" (\"a\" INTEGER, \"b\" INTEGER, \"c\" TEXT, PRIMARY KEY (\"a\", \"b\"), UNIQUE (\"c\"), CHECK ((\"a\" > 0 AND \"b\" > 0)), CONSTRAINT \"fk_a\" FOREIGN KEY (\"a\") REFERENCES \"p\"(\"id\") ON DELETE CASCADE ON UPDATE NO ACTION, FOREIGN KEY (\"b\", \"c\") REFERENCES \"q\"(\"x\", \"y\") DEFERRABLE INITIALLY DEFERRED, UNIQUE (\"a\", \"c\") ON CONFLICT IGNORE) STRICT"
         (one {:create-table :m
               :columns [[:a :integer] [:b :integer] [:c :text]]
               :primary-key [:a :b]
               :unique [[:c]]
               :constraints [[:unique [:a :c] {:on-conflict :ignore}]]
               :check [[:and [:> :a 0] [:> :b 0]]]
               :foreign-keys [{:columns [:a] :table :p :ref-columns [:id] :on-delete :cascade :on-update :no-action :name :fk-a}
                              {:columns [:b :c] :table :q :ref-columns [:x :y] :deferrable :initially-deferred}]})))
  (is (= "CREATE TABLE IF NOT EXISTS \"s\" (\"id\" INTEGER CONSTRAINT \"pk\" PRIMARY KEY DESC ON CONFLICT REPLACE AUTOINCREMENT, \"n\" TEXT NOT NULL ON CONFLICT IGNORE, \"u\" TEXT UNIQUE ON CONFLICT FAIL) STRICT"
         (one {:create-table :s
               :columns [[:id :integer {:primary-key? :desc :autoincrement? true :on-conflict :replace :name :pk}]
                         [:n :text {:not-null? true :on-conflict :ignore}]
                         [:u :text {:unique? true :on-conflict :fail}]]}))
      "conflict clauses, constraint names, AUTOINCREMENT only on request"))

(deftest strict-type-enforcement
  (testing "STRICT (default) rejects everything outside the six types, naming the column"
    (is (= {:column :flag :column-type :boolean :type :sqlite-cljs/argument-error}
           (select-keys (err #(ddl/render {:create-table :t :columns [[:id :integer] [:flag :boolean {}]]})) [:column :column-type :type])))
    (is (arg-error? #(ddl/render {:create-table :t :columns [[:flag :boolean {}]]})))
    (is (arg-error? #(ddl/render {:create-table :t :columns [[:at :datetime {}]]})))
    (is (arg-error? #(ddl/render {:create-table :t :columns [[:s :varchar {}]]})))
    (is (arg-error? #(ddl/render {:create-table :t :columns [[:s [:raw "VARCHAR(10)"] {}]]})) "raw types need :strict? false")
    (is (arg-error? #(ddl/render {:create-table :t :columns [[:n :numeric {}]]})) "NUMERIC needs :strict? false")
    (is (= :missing-type (:reason (err #(ddl/render {:create-table :t :columns [[:n nil {}]]}))))))
  (testing "non-strict still rejects :boolean/:datetime (only :raw spells legacy names)"
    (is (arg-error? #(ddl/render {:create-table :t :strict? false :columns [[:flag :boolean {}]]})))
    (is (= "CREATE TABLE IF NOT EXISTS \"t\" (\"flag\" BOOLEAN)"
           (one {:create-table :t :strict? false :columns [[:flag [:raw "BOOLEAN"] {}]]})))))

(deftest column-validation
  (is (arg-error? #(ddl/render {:create-table :t :columns [[:a :integer {:primary-key? true}] [:b :integer {:primary-key? true}]]}))
      "two column PKs: use composite :primary-key")
  (is (arg-error? #(ddl/render {:create-table :t :columns [[:a :integer {:primary-key? true}]] :primary-key [:a]})))
  (is (arg-error? #(ddl/render {:create-table :t :columns [[:a :text {:primary-key? true :autoincrement? true}]]}))
      "AUTOINCREMENT needs INTEGER PRIMARY KEY")
  (is (arg-error? #(ddl/render {:create-table :t :without-rowid? true :columns [[:a :integer {:primary-key? true :autoincrement? true}]]})))
  (is (arg-error? #(ddl/render {:create-table :t :without-rowid? true :columns [[:a :integer]]})) "WITHOUT ROWID needs a PK")
  (is (arg-error? #(ddl/render {:create-table :t :columns [[:a :integer {:generated-as {:expr [:+ 1 1]} :default 1}]]})))
  (is (arg-error? #(ddl/render {:create-table :t :columns [[:a :integer {:generated-as {:expr [:+ 1 1]}}]]})) "at least one non-generated column")
  (is (arg-error? #(ddl/render {:create-table :t :columns [[:a :integer {:not-nul true}]]})) "unknown option")
  (is (arg-error? #(ddl/render {:create-table :t :columns [[:a :integer] [:A :integer]]})) "duplicate (case-insensitive) names")
  (is (arg-error? #(ddl/render {:create-table :t :columns [[:a :integer {:default :now}]]})) "default keywords are allow-listed")
  (is (arg-error? #(ddl/render {:create-table :t :columns [[:a :integer {:references {:column :id}}]]})))
  (is (arg-error? #(ddl/render {:create-table :t :columns [[:a :integer {:references {:table :p :on-delete :explode}}]]})))
  (is (arg-error? #(ddl/render {:create-table :t :columns []})))
  (is (arg-error? #(ddl/render {:create-table :t :columns [[:a :integer]] :colums []})) "unknown table key"))

(deftest defaults-are-literals-never-parameters
  (is (= "CREATE TABLE IF NOT EXISTS \"t\" (\"a\" TEXT DEFAULT 'it''s', \"b\" INTEGER DEFAULT 0, \"c\" REAL DEFAULT 1.5, \"d\" TEXT DEFAULT NULL, \"e\" INTEGER DEFAULT (abs(-1)), \"f\" TEXT DEFAULT CURRENT_DATE) STRICT"
         (one {:create-table :t
               :columns [[:a :text {:default "it's"}] [:b :integer {:default 0}] [:c :real {:default 1.5}]
                         [:d :text {:default nil}] [:e :integer {:default [:abs -1]}] [:f :text {:default :current-date}]]})))
  (let [evil "x'); DROP TABLE t; --"]
    (is (= "CREATE TABLE IF NOT EXISTS \"t\" (\"a\" TEXT DEFAULT 'x''); DROP TABLE t; --') STRICT"
           (one {:create-table :t :columns [[:a :text {:default evil}]]}))
        "string defaults are single-quote escaped")))

(deftest create-index-golden
  (is (= "CREATE UNIQUE INDEX IF NOT EXISTS \"users_org_created_idx\" ON \"users\" (\"org_id\", \"created_at\" DESC, lower(\"email\") COLLATE NOCASE ASC) WHERE \"active\" = 1"
         (one {:create-index :users-org-created-idx :on :users
               :columns [:org-id [:created-at :desc] [[:lower :email] :asc [:collate :nocase]]]
               :unique? true :where [:= :active 1]})))
  (is (= "CREATE INDEX \"t_a_idx\" ON \"t\" (\"a\")" (one {:create-index nil :table :t :columns [:a] :if-not-exists? false})))
  (is (= "CREATE INDEX IF NOT EXISTS \"i\" ON \"t\" ((\"a\" + \"b\"))" (one {:create-index :i :on :t :columns [[[:+ :a :b]]]})))
  (is (= "CREATE INDEX IF NOT EXISTS \"i\" ON \"t\" (lower(\"a\"))" (one {:create-index :i :on :t :columns [[:lower :a]]}))
      "an operator vector is an expression")
  (is (= "CREATE INDEX IF NOT EXISTS \"i\" ON \"t\" (\"a\") WHERE \"b\" IS NOT NULL"
         (one {:create-index :i :on :t :columns [:a] :partial-where [:not= :b nil]})))
  (is (arg-error? #(ddl/render {:create-index :i :columns [:a]})) "needs :on")
  (is (arg-error? #(ddl/render {:create-index :i :on :t})) "needs :columns"))

(deftest create-view-and-trigger
  (is (= "CREATE VIEW IF NOT EXISTS \"active_users\" (\"id\", \"name\") AS SELECT \"id\", \"name\" FROM \"users\" WHERE \"active\" = 1"
         (one {:create-view :active-users :columns [:id :name]
               :as {:select [:id :name] :from :users :where [:= :active 1]}}))
      "view bodies render in literal mode (no parameters allowed)")
  (is (= "CREATE TEMP VIEW \"v\" AS SELECT * FROM \"t\"" (one {:create-view :v :temp? true :if-not-exists? false :as {:select :* :from :t}})))
  (is (= "CREATE TRIGGER IF NOT EXISTS \"users_touch\" AFTER UPDATE OF \"name\", \"email\" ON \"users\" FOR EACH ROW WHEN \"new\".\"id\" > 0 BEGIN UPDATE users SET updated_at = unixepoch() WHERE id = NEW.id; UPDATE \"log\" SET \"n\" = (\"n\" + 1) WHERE \"id\" = 1; END"
         (one {:create-trigger :users-touch :on :users :timing :after :event [:update :of [:name :email]]
               :for-each-row? true :when [:> :new/id 0]
               :body ["UPDATE users SET updated_at = unixepoch() WHERE id = NEW.id"
                      {:update :log :set {:n [:+ :n 1]} :where [:= :id 1]}]}))
      "raw body strings (house position) plus DSL maps in literal mode")
  (is (= "CREATE TRIGGER IF NOT EXISTS \"t_del\" DELETE ON \"t\" BEGIN SELECT RAISE(ABORT, 'no'); END"
         (one {:create-trigger :t-del :on :t :event :delete :body [{:raw "SELECT RAISE(ABORT, 'no')"}]})))
  (is (arg-error? #(ddl/render {:create-trigger :x :on :t :event :truncate :body ["x"]})))
  (is (arg-error? #(ddl/render {:create-trigger :x :on :t :event :insert :body []}))))

(deftest alter-table-golden-and-gates
  (is (= "ALTER TABLE \"users\" RENAME TO \"people\"" (one {:alter-table :users :rename-to :people})))
  (is (= "ALTER TABLE \"users\" RENAME COLUMN \"name\" TO \"full_name\"" (one {:alter-table :users :rename-column [:name :full-name]})))
  (is (= "ALTER TABLE \"users\" ADD COLUMN \"nickname\" TEXT NOT NULL DEFAULT ''"
         (one {:alter-table :users :add-column [:nickname :text {:default "" :not-null? true}]})))
  (is (= "ALTER TABLE \"users\" DROP COLUMN \"legacy\"" (one {:alter-table :users :drop-column :legacy})))
  (is (= "ALTER TABLE \"users\" ALTER COLUMN \"name\" SET NOT NULL" (one {:alter-table :users :alter-column [:name :set-not-null]})))
  (is (= "ALTER TABLE \"users\" ALTER COLUMN \"name\" DROP NOT NULL" (one {:alter-table :users :alter-column [:name :drop-not-null]})))
  (testing "version gates (inventory §8.1) only when the version is known"
    (is (= {:statements ["ALTER TABLE \"u\" DROP COLUMN \"x\""]} (ddl/render {:alter-table :u :drop-column :x} {:sqlite-version "3.53.4"})))
    (is (= "3.35.0" (:requires (err #(ddl/render {:alter-table :u :drop-column :x} {:sqlite-version "3.34.1"})))))
    (is (= "3.53.0" (:requires (err #(ddl/render {:alter-table :u :alter-column [:x :set-not-null]} {:sqlite-version "3.51.3"})))))
    (is (= "3.25.0" (:requires (err #(ddl/render {:alter-table :u :rename-column [:a :b]} {:sqlite-version "3.24.0"}))))))
  (testing "ADD COLUMN restrictions (inventory §3.13)"
    (is (arg-error? #(ddl/render {:alter-table :u :add-column [:x :integer {:primary-key? true}]})))
    (is (arg-error? #(ddl/render {:alter-table :u :add-column [:x :integer {:unique? true}]})))
    (is (arg-error? #(ddl/render {:alter-table :u :add-column [:x :integer {:not-null? true}]})) "NOT NULL needs a default")
    (is (arg-error? #(ddl/render {:alter-table :u :add-column [:x :text {:default :current-timestamp}]})))
    (is (arg-error? #(ddl/render {:alter-table :u :add-column [:x :text {:default [:raw "(unixepoch())"]}]})))
    (is (arg-error? #(ddl/render {:alter-table :u :add-column [:x :text {:generated-as {:expr [:upper :a] :stored? true}}]})))
    (is (arg-error? #(ddl/render {:alter-table :u :add-column [:x :boolean {}]})) "strict type check applies"))
  (is (arg-error? #(ddl/render {:alter-table :u :rename-to :v :drop-column :x})) "one action per map")
  (is (arg-error? #(ddl/render {:alter-table :u}))))

(deftest drop-golden
  (is (= "DROP TABLE IF EXISTS \"users\"" (one {:drop-table :users})))
  (is (= "DROP TABLE \"users\"" (one {:drop-table :users :if-exists? false})))
  (is (= "DROP INDEX IF EXISTS \"users_email_idx\"" (one {:drop-index :users-email-idx})))
  (is (= "DROP VIEW IF EXISTS \"v\"" (one {:drop-view :v})))
  (is (= "DROP TRIGGER IF EXISTS \"tg\"" (one {:drop-trigger :tg})))
  (is (= "DROP TABLE IF EXISTS \"main\".\"users\"" (one {:drop-table :main/users})) "schema-qualified names"))

(deftest def-kind-and-name
  (is (= :create-table (ddl/def-kind {:create-table :t :columns [[:a :integer]]})))
  (is (= :users (ddl/def-name {:create-table :users :columns []})))
  (is (= "users_a_b_idx" (ddl/def-name {:create-index nil :on :users :columns [:a [:b :desc]]})))
  (is (nil? (ddl/def-kind {:select :*})))
  (is (arg-error? #(ddl/render {:select :*})))
  (is (= [{:name "id" :type "INTEGER"} {:name "legacy" :type "VARCHAR(9)"}]
         (ddl/expected-columns {:create-table :t :strict? false :columns [[:id :integer {}] [:legacy [:raw "VARCHAR(9)"] {}]]}))))

(deftest identifier-injection-in-ddl
  (let [s (one {:create-table "users\"; DROP TABLE x; --" :columns [["a\"b" :integer {}]]})]
    (is (str/starts-with? s "CREATE TABLE IF NOT EXISTS \"users\"\"; DROP TABLE x; --\" (\"a\"\"b\" INTEGER"))))
