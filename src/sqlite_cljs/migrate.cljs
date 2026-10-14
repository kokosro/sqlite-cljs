(ns sqlite-cljs.migrate
  "Idempotent schema apply: (migrate! db defs) takes an ORDERED vector of
   DDL definition maps (sqlite-cljs.ddl) and brings the database up to
   them, creating what is absent and leaving what exists alone.

   Per definition kind:
   * :create-table — absent: created (with its :indexes); present: left
     alone (its :indexes are still created if absent). After EVERY def has
     been applied, each table def is verified against PRAGMA table_xinfo: a
     declared column that is missing, or whose declared type differs, is
     DRIFT and the whole migrate! rejects (rolling everything back) with
     :sqlite-cljs/migration-error {:reason :drift :table .. :missing [..]
     :type-mismatch [..]}. Nothing is altered automatically. Extra columns
     in the database are allowed.

     Evolving a table: keep the table def describing the DESIRED shape and
     add explicit :alter-table defs that bring existing databases there —
     a fresh database gets the new shape from CREATE TABLE and skips the
     alters, an old database applies them, and both verify clean:

       [{:alter-table :orgs :rename-to :organisations}   ; renames go BEFORE the renamed def
        {:create-table :organisations ...}
        {:create-table :users :columns [.. [:nickname :text {}] ..]}
        {:alter-table :users :add-column [:nickname :text {}]}
        {:alter-table :users :rename-column [:email :mail]}
        {:create-index :users-mail-idx :on :users :columns [:mail]}]
   * :create-index / :create-view / :create-trigger — created if absent
     (by name in sqlite_master), otherwise reported as existing.
   * :alter-table — applied only when its effect is not yet there
     (:add-column when the column is missing, :rename-column/:rename-to
     when the old name still exists and the new one does not, :drop-column
     when the column exists, :alter-column always), so re-running is a
     no-op. A :rename-to whose source table is absent is skipped (a fresh
     database creates the renamed table directly); any other alter on a
     missing table is a migration-error {:reason :no-such-table}.
   * a statement SQLite refuses rejects with {:reason :statement-failed
     :index i :def .. :code \"SQLITE_..\" :cause <sqlite-error>}.
   * :drop-table/-index/-view/-trigger — applied when the object exists.

   Everything runs in ONE BEGIN IMMEDIATE transaction (DDL is transactional
   in SQLite): a failure leaves schema and user_version untouched.

   user_version semantics: after a successful run PRAGMA user_version is set
   to (count defs) as a cheap fingerprint of \"how many definitions this
   database has been brought up to\". It is not a migration cursor: every
   run re-checks every definition. If the database's user_version is
   greater than (count defs) the code is older than the database and
   migrate! rejects with {:reason :database-newer} unless :allow-newer? true.

   Returns a promise of the report
   {:created [names] :existing [names] :altered [names] :dropped [names]
    :user-version n :previous-user-version n :sqlite-version \"..\"}.
   The statement cache is cleared afterwards."
  (:require [clojure.string :as str]
            [promesa.core :as p]
            [sqlite-cljs.core :as core]
            [sqlite-cljs.ddl :as ddl]
            [sqlite-cljs.util :as util]))

;; ---------------------------------------------------------------------------
;; introspection (synchronous internals)
;; ---------------------------------------------------------------------------

(defn- master-row [d nm]
  (core/get* d "SELECT type, name, tbl_name AS table_name, sql FROM sqlite_master WHERE name = ? COLLATE NOCASE"
             [(util/ident-name nm)]))

(defn object-type*
  "\"table\" | \"index\" | \"view\" | \"trigger\" | nil for a schema object name.
   Synchronous internal."
  [d nm]
  (:type (master-row d nm)))

(defn table-columns*
  "PRAGMA table_xinfo rows for a table: [{:cid :name :type :notnull
   :dflt-value :pk :hidden} ..]. Synchronous internal."
  [d table]
  (core/pragma* d :table-xinfo {:arg (util/ident-name table) :rows? true}))

(defn- column-names [d table]
  (into #{} (map (comp str/lower-case :name)) (table-columns* d table)))

(defn- run-statements! [d statements]
  (doseq [s statements] (core/run* d s [])))

;; ---------------------------------------------------------------------------
;; drift
;; ---------------------------------------------------------------------------

(defn- drift [def actual]
  (let [expected (ddl/expected-columns def)
        by-name (into {} (map (fn [c] [(str/lower-case (:name c)) c])) actual)
        missing (vec (remove #(contains? by-name (str/lower-case (:name %))) expected))
        mismatched (vec (keep (fn [{:keys [name type]}]
                                (when-let [a (get by-name (str/lower-case name))]
                                  (when (not= (str/upper-case (str (:type a))) (str/upper-case type))
                                    {:column name :expected type :actual (:type a)})))
                              expected))]
    (when (or (seq missing) (seq mismatched))
      {:missing (mapv :name missing) :type-mismatch mismatched})))

;; ---------------------------------------------------------------------------
;; apply one definition
;; ---------------------------------------------------------------------------

(defn- apply-def! [d def opts report]
  (let [kind (ddl/def-kind def)
        nm (ddl/def-name def)
        {:keys [statements]} (ddl/render def opts)
        existing (object-type* d nm)
        add (fn [k] (update report k conj nm))]
    (case kind
      :create-table
      (if existing
        (do (when-not (= "table" existing)
              (throw (util/migration-error (str "cannot create table " (pr-str nm) ": a " existing " with that name exists")
                                           {:reason :name-clash :name nm :existing existing})))
            ;; drift is verified after every def has been applied (verify-tables!);
            ;; the table's indexes are still created when absent
            (let [idx-stmts (rest statements)
                  idx-defs (:indexes def)
                  report (add :existing)]
              (reduce (fn [r [[inm cols] stmt]]
                        (let [iname (or inm (ddl/def-name {:create-index nil :on nm :columns cols}))]
                          (if (object-type* d iname)
                            (update r :existing conj iname)
                            (do (core/run* d stmt []) (update r :created conj iname)))))
                      report
                      (map vector idx-defs idx-stmts))))
        (do (run-statements! d statements)
            (reduce (fn [r [inm cols]] (update r :created conj (or inm (ddl/def-name {:create-index nil :on nm :columns cols}))))
                    (add :created)
                    (:indexes def))))

      (:create-index :create-view :create-trigger)
      (if existing
        (add :existing)
        (do (run-statements! d statements) (add :created)))

      :alter-table
      (let [table (:alter-table def)
            cols (when (object-type* d table) (column-names d table))
            lower (fn [x] (str/lower-case (util/ident-name x)))
            needed? (cond
                      (contains? def :rename-to) (and (object-type* d table) (not (object-type* d (:rename-to def))))
                      (contains? def :rename-column) (let [[old new] (:rename-column def)]
                                                       (and cols (contains? cols (lower old)) (not (contains? cols (lower new)))))
                      (contains? def :add-column) (and cols (not (contains? cols (lower (first (:add-column def))))))
                      (contains? def :drop-column) (and cols (contains? cols (lower (:drop-column def))))
                      (contains? def :alter-column) (boolean cols)
                      :else false)]
        (when (and (nil? cols) (not (contains? def :rename-to)))
          (throw (util/migration-error (str "cannot alter " (pr-str table) ": no such table") {:reason :no-such-table :table table})))
        (if needed?
          (do (run-statements! d statements) (add :altered))
          (add :existing)))

      (:drop-table :drop-index :drop-view :drop-trigger)
      (if existing
        (do (run-statements! d statements) (add :dropped))
        (add :existing))

      (throw (util/migration-error (str "not a DDL definition: " (pr-str def)) {:reason :bad-def :def def})))))

(defn- verify-tables!
  "After every def is applied: each :create-table def must exist as a table
   with all its declared columns and types (extra columns are fine)."
  [d defs]
  (doseq [def defs :when (= :create-table (ddl/def-kind def))]
    (let [nm (:create-table def)
          existing (object-type* d nm)]
      (when-not (= "table" existing)
        (throw (util/migration-error (str "table " (pr-str nm) " does not exist after applying the definitions (renamed away by a later :alter-table?)")
                                     {:reason :drift :table nm :missing-table true})))
      (when-let [dr (drift def (table-columns* d nm))]
        (throw (util/migration-error
                (str "table " (pr-str nm) " drifted from its definition: missing " (pr-str (:missing dr))
                     ", type mismatches " (pr-str (:type-mismatch dr))
                     " — add explicit :alter-table defs or migrate by hand")
                (merge {:reason :drift :table nm} dr)))))))

;; ---------------------------------------------------------------------------
;; public
;; ---------------------------------------------------------------------------

(defn validate
  "Pure: problems with a defs vector as a vector of {:index :problem}
   strings (empty when fine): must be a vector of DDL definition maps that
   each render."
  [defs]
  (if-not (sequential? defs)
    [{:index nil :problem "defs must be a vector of DDL definition maps"}]
    (vec (keep-indexed
          (fn [i def]
            (cond
              (not (map? def)) {:index i :problem (str "not a map: " (pr-str def))}
              (nil? (ddl/def-kind def)) {:index i :problem (str "not a DDL definition: " (pr-str (keys def)))}
              :else (try (ddl/render def) nil
                         (catch :default e {:index i :problem (ex-message e) :error e}))))
          defs))))

(defn migrate!
  "Applies `defs` (ordered vector of DDL definition maps) idempotently; see
   the namespace docstring. opts: :allow-newer? (default false), :log (fn of
   one string, called per applied statement group). Resolves to the report
   map; rejects with :sqlite-cljs/migration-error on drift/version problems
   and on a statement SQLite refuses (nothing is applied in that case)."
  ([d defs] (migrate! d defs {}))
  ([d defs {:keys [allow-newer? log]}]
   (try
     (let [problems (validate defs)]
       (when (seq problems)
         (throw (util/migration-error (str "invalid defs: " (:problem (first problems)))
                                      {:reason :invalid-defs :problems problems :cause (:error (first problems))})))
       (let [version (core/sqlite-version* d)
             previous (core/pragma* d :user-version)
             target (count defs)]
         (when (and (> previous target) (not allow-newer?))
           (throw (util/migration-error
                   (str "database user_version " previous " is above the " target " definitions given (the code is older than the database); pass :allow-newer? true to proceed")
                   {:reason :database-newer :user-version previous :defs target})))
         (let [report (core/transaction*
                       d :immediate
                       (fn [_]
                         (let [r (reduce (fn [report [i def]]
                                           (let [r (try (apply-def! d def {:sqlite-version version} report)
                                                        (catch :default e
                                                          (if (= :sqlite-cljs/sqlite-error (util/error-type e))
                                                            (throw (util/migration-error
                                                                    (str "definition #" i " (" (name (ddl/def-kind def)) " " (pr-str (ddl/def-name def))
                                                                         ") failed: " (ex-message e))
                                                                    {:reason :statement-failed :index i :def def :cause e
                                                                     :code (util/sqlite-code e)}))
                                                            (throw e))))]
                                             (when log (log (str "migrate! " (name (ddl/def-kind def)) " " (pr-str (ddl/def-name def)))))
                                             r))
                                         {:created [] :existing [] :altered [] :dropped []}
                                         (map-indexed vector defs))]
                           (verify-tables! d defs)
                           (core/pragma* d :user-version {:value target})
                           r)))]
           (core/clear-cache! d)
           (p/resolved (assoc report
                              :user-version target
                              :previous-user-version previous
                              :sqlite-version version)))))
     (catch :default e
       (core/clear-cache! d)
       (p/rejected e)))))

(defn status!
  "Resolves to {:user-version n :schema-version n :tables [names] :views
   [names] :indexes [names] :triggers [names]} from PRAGMA user_version /
   schema_version and sqlite_master."
  [d]
  (try
    (let [rows (core/all* d "SELECT type, name FROM sqlite_master WHERE name NOT LIKE 'sqlite_%' ORDER BY type, name" [])
          by-type (group-by :type rows)
          names (fn [t] (mapv :name (get by-type t)))]
      (p/resolved {:user-version (core/pragma* d :user-version)
                   :schema-version (core/pragma* d :schema-version)
                   :tables (names "table")
                   :views (names "view")
                   :indexes (names "index")
                   :triggers (names "trigger")}))
    (catch :default e (p/rejected e))))
