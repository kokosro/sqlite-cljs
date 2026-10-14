(ns sqlite-cljs.core
  "The ONE driver-binding namespace: every line of better-sqlite3 interop in
   the library lives here. Nothing else in
   sqlite-cljs (and nothing at call sites) touches a driver object.

   A *db value* is a plain map returned by `db`:

     {:db        <raw driver connection — opaque, never use it directly>
      :path      \":memory:\" or the file path
      :memory?   true for in-memory databases
      :open-opts the options `db` was called with
      :effective {:journal-mode .. :synchronous .. :foreign-keys? ..
                  :busy-timeout-ms .. :trusted-schema? .. :safe-integers? ..}
      :cache     <statement cache, an LRU keyed by SQL text>}

   The db value IS the connection: one per process/database.
   Pass it to every function in sqlite-cljs.query / migrate.

   Async policy: better-sqlite3 is synchronous, so every
   promise returned here is *already settled* when you receive it. The
   promise API is kept so that a future asynchronous driver binding can be
   swapped in without changing call sites. Inside a `sqlite-cljs.query/transact!`
   body the settled promises can be dereferenced (`@(query/execute! tx q)`)
   to get the value synchronously; a rejected promise throws on deref,
   which is what makes the transaction wrapper roll back.

   Value binding: nil -> NULL; booleans -> INTEGER
   1/0; integral numbers within ±2^53-1 -> INTEGER (converted to BigInt
   internally, because the driver would otherwise bind every JS number as
   REAL and store '5.0' in TEXT columns); other finite numbers -> REAL;
   NaN/±Infinity -> argument-error; BigInt -> INTEGER; strings -> TEXT (NUL
   rejected); keywords -> their name as TEXT; Buffer/Uint8Array -> BLOB
   (opaque); js/Date, collections and other objects -> argument-error.

   Reads: INTEGER -> number (BigInt when :safe-integers? is on),
   REAL -> number, TEXT -> string, BLOB -> Buffer, NULL -> nil; row keys are
   kebab-case keywords. No boolean/date reconstruction unless a :coerce map
   is given (see `coerce-row`)."
  (:require [clojure.string :as str]
            [promesa.core :as p]
            [sqlite-cljs.util :as util]))

;; ---------------------------------------------------------------------------
;; Driver loading
;; ---------------------------------------------------------------------------

(def ^:private driver-name "better-sqlite3")

(def ^:private driver
  (delay
    (try
      (js/require driver-name)
      (catch :default e
        (throw (util/config-error
                (str "cannot load the SQLite driver `" driver-name
                     "` — run `npm install " driver-name "` (Node >= 22 required)")
                {:reason :driver-missing :cause e}))))))

(def ^:private driver-version
  (delay (unchecked-get (js/require (str driver-name "/package.json")) "version")))

(def default-cache-size
  "Default number of prepared statements kept per connection."
  128)

(def max-params
  "SQLITE_MAX_VARIABLE_NUMBER of the bundled SQLite: the
   maximum number of bound parameters in one statement."
  32766)

(def max-safe-integer
  "2^53-1: the largest integral JS number that is bound exactly."
  js/Number.MAX_SAFE_INTEGER)

;; ---------------------------------------------------------------------------
;; Error mapping
;; ---------------------------------------------------------------------------

(defn- sqlite-error? [e]
  (and (some? e)
       (= "SqliteError" (unchecked-get e "name"))
       (string? (unchecked-get e "code"))))

(defn wrap-error
  "Maps anything thrown by the driver to a sqlite-cljs ex-info:
   SqliteError -> :sqlite-cljs/sqlite-error {:code \"SQLITE_..\" :message ..
   :sql ..}; RangeError/TypeError (binding count, unsupported value, closed
   connection, multi-statement string, non-reader statement) ->
   :sqlite-cljs/argument-error with :reason; sqlite-cljs errors pass
   through; anything else is returned untouched."
  [e sql]
  (cond
    (util/error-data e) e
    (sqlite-error? e) (util/sqlite-error e {:sql sql})
    (or (instance? js/RangeError e) (instance? js/TypeError e))
    (let [msg (str (unchecked-get e "message"))
          reason (cond
                   (str/includes? msg "not open") :closed
                   (str/includes? msg "more than one statement") :multiple-statements
                   (str/includes? msg "does not return data") :not-a-reader
                   (str/includes? msg "parameter") :parameter-count
                   (str/includes? msg "can only bind") :unbindable-value
                   (str/includes? msg "too big") :value-too-big
                   :else :driver-misuse)]
      (util/argument-error msg (cond-> {:reason reason :cause e} sql (assoc :sql sql))))
    :else e))

;; ---------------------------------------------------------------------------
;; Values -> driver values
;; ---------------------------------------------------------------------------

(defn- bigint? [v] (= "bigint" (js* "typeof ~{}" v)))

(defn ->param
  "Converts one ClojureScript value into a driver-bindable value (see the
   namespace docstring for the table). Throws :sqlite-cljs/argument-error
   with :reason for anything that cannot be bound losslessly."
  [v]
  (cond
    (nil? v) nil
    (true? v) (js/BigInt 1)
    (false? v) (js/BigInt 0)
    (number? v)
    (cond
      (js/isNaN v) (throw (util/argument-error "cannot bind NaN" {:reason :nan :value v}))
      (not (js/isFinite v)) (throw (util/argument-error "cannot bind Infinity" {:reason :infinity :value v}))
      (not (js/Number.isInteger v)) v
      (> (js/Math.abs v) max-safe-integer)
      (throw (util/argument-error
              (str "integer " v " exceeds 2^53-1; pass a BigInt for exact binding")
              {:reason :unsafe-integer :value v}))
      :else (js/BigInt v))
    (bigint? v) v
    (string? v)
    (if (str/includes? v "\u0000")
      (throw (util/argument-error "cannot bind a string containing a NUL character"
                                  {:reason :nul :value v}))
      v)
    (keyword? v) (name v)
    (instance? js/Uint8Array v) v
    (instance? js/ArrayBuffer v) (js/Uint8Array. v)
    (js/ArrayBuffer.isView v) (js/Uint8Array. (.-buffer v) (.-byteOffset v) (.-byteLength v))
    (instance? js/Date v)
    (throw (util/argument-error
            "cannot bind a js/Date: store epoch milliseconds (.getTime d) in an INTEGER column, or an ISO string"
            {:reason :date :value v}))
    (or (coll? v) (symbol? v))
    (throw (util/argument-error
            (str "cannot bind a collection/symbol as a value: " (pr-str v)
                 " (serialize it to a string first)")
            {:reason :collection :value v}))
    :else
    (throw (util/argument-error (str "cannot bind value of this type: " (pr-str v))
                                {:reason :unbindable-value :value v}))))

(defn ->params
  "Vector/seq of values -> JS array of driver values (see ->param). The
   count is checked against `max-params`."
  [params]
  (let [params (or params [])]
    (when (> (count params) max-params)
      (throw (util/argument-error
              (str (count params) " parameters exceed SQLITE_MAX_VARIABLE_NUMBER (" max-params ")")
              {:reason :too-many-parameters :count (count params)})))
    (let [arr (array)]
      (doseq [v params] (.push arr (->param v)))
      arr)))

;; ---------------------------------------------------------------------------
;; Reads: value helpers and :coerce
;; ---------------------------------------------------------------------------

(defn bigint?*
  "True when `v` is a JS BigInt (what INTEGER columns read as under
   :safe-integers? true)."
  [v]
  (bigint? v))

(defn bigint->number
  "BigInt -> number when it is within ±2^53-1, otherwise throws
   :sqlite-cljs/argument-error (:reason :unsafe-integer). Numbers and nil
   pass through, so it is safe to apply to any INTEGER read."
  [v]
  (cond
    (nil? v) nil
    (number? v) v
    (bigint? v)
    (let [n (js/Number v)]
      (if (> (js/Math.abs n) max-safe-integer)
        (throw (util/argument-error (str "BigInt " (str v) " does not fit in a JS number")
                                    {:reason :unsafe-integer :value v}))
        n))
    :else (throw (util/argument-error (str "not an integer: " (pr-str v)) {:value v}))))

(defn- coerce-value [how v]
  (cond
    (nil? v) nil
    (fn? how) (how v)
    :else
    (case how
      :boolean (cond (bigint? v) (not= (js/Number v) 0)
                     (number? v) (not= v 0)
                     :else (boolean v))
      :date (js/Date. (if (bigint? v) (js/Number v) v))
      :number (bigint->number v)
      :bigint (if (bigint? v) v (js/BigInt v))
      :json (js->clj (js/JSON.parse v) :keywordize-keys true)
      :keyword (keyword v)
      (throw (util/argument-error (str "unknown :coerce kind " (pr-str how))
                                  {:reason :coerce :value how})))))

(defn coerce-row
  "Applies a :coerce map {column-keyword kind-or-fn} to one row map. Kinds:
   :boolean (INTEGER 0/1 -> false/true), :date (epoch-ms INTEGER -> js/Date),
   :number (BigInt -> number), :bigint, :json (TEXT -> data, keywordized),
   :keyword, or any 1-arg fn. NULL (nil) is never coerced. Opt-in per
   query: rows are otherwise returned raw."
  [coerce row]
  (if (empty? coerce)
    row
    (reduce-kv (fn [r k how]
                 (if (contains? r k) (assoc r k (coerce-value how (get r k))) r))
               row coerce)))

;; ---------------------------------------------------------------------------
;; Statement cache: a js/Map (insertion ordered) used as an LRU
;; ---------------------------------------------------------------------------

(defn- cache-get [m sql]
  (when-let [st (.get m sql)]
    (.delete m sql)
    (.set m sql st)
    st))

(defn- cache-put [m sql st limit]
  (.set m sql st)
  (when (> (.-size m) limit)
    (.delete m (.-value (.next (.keys m)))))
  st)

(defn clear-cache!
  "Drops every cached prepared statement of the connection (called by
   migrate! and exec! after schema changes). Synchronous."
  [d]
  (.clear (:cache d))
  nil)

(defn cache-size
  "Number of prepared statements currently cached for the connection."
  [d]
  (.-size (:cache d)))

(defn cached?
  "True when the exact SQL text is in the connection's statement cache."
  [d sql]
  (.has (:cache d) sql))

;; ---------------------------------------------------------------------------
;; Connection
;; ---------------------------------------------------------------------------

(defn- conn [d]
  (or (:db d)
      (throw (util/argument-error "not a db value (expected the map returned by sqlite-cljs.core/db)"
                                  {:reason :not-a-db :value d}))))

(defn- pragma-ident [k]
  (let [s (util/kebab->snake k)]
    (when-not (re-matches #"[A-Za-z_][A-Za-z0-9_]*" s)
      (throw (util/argument-error (str "not a PRAGMA name: " (pr-str k)) {:value k})))
    s))

(defn- pragma-value [v]
  (cond
    (true? v) "1"
    (false? v) "0"
    (integer? v) (str v)
    (keyword? v) (str/upper-case (util/kebab->snake v))
    (string? v) (if (re-matches #"[A-Za-z0-9_\-]+" v)
                  v
                  (throw (util/argument-error (str "PRAGMA value must be a bare word: " (pr-str v)) {:value v})))
    :else (throw (util/argument-error (str "unsupported PRAGMA value: " (pr-str v)) {:value v}))))

(defn- pragma-text [name-kw {:keys [arg value]}]
  (str (pragma-ident name-kw)
       (when (some? arg) (str "(" (util/sql-ident arg) ")"))
       (when (some? value) (str " = " (pragma-value value)))))

(defn pragma*
  "Synchronous PRAGMA. opts: :arg (identifier argument, e.g. table name),
   :value (setter), :rows? true to get all rows (kebab maps) instead of the
   single value of the first row. Internal; the promise API is `pragma` /
   `pragma!`."
  ([d name-kw] (pragma* d name-kw {}))
  ([d name-kw {:keys [rows?] :as opts}]
   (let [text (pragma-text name-kw opts)]
     (try
       (let [r (js-invoke (conn d) "pragma" text #js {:simple (not rows?)})]
         (if rows? (util/rows->clj r) r))
       (catch :default e (throw (wrap-error e (str "PRAGMA " text))))))))

(defn- open-connection [path {:keys [readonly? file-must-exist? timeout-ms]}]
  (try
    (let [Database @driver]
      (Database. path #js {:readonly (boolean readonly?)
                           :fileMustExist (boolean file-must-exist?)
                           :timeout timeout-ms}))
    (catch :default e
      (cond
        (sqlite-error? e) (throw (util/sqlite-error e {:sql (str "open " path)}))
        (util/error-data e) (throw e)
        :else (throw (util/config-error (str "cannot open " (pr-str path) ": " (unchecked-get e "message"))
                                        {:reason :open-failed :path path :cause e}))))))

(def journal-modes
  "Accepted :journal-mode values."
  #{:wal :delete :truncate :persist :memory :off})

(def synchronous-modes
  "Accepted :synchronous values (the numeric forms 0-3 are accepted too)."
  #{:off :normal :full :extra})

(defn db
  "Opens a connection and returns the db value (see the namespace docstring).
   `path-or-opts` is a path string, \":memory:\", or an options map:

     :path              file path (or :memory? true; default :memory:)
     :memory?           true for an in-memory database
     :readonly?         open read-only (file databases only; writes raise
                        SQLITE_READONLY)
     :file-must-exist?  fail with SQLITE_CANTOPEN instead of creating the file
     :timeout-ms        busy_timeout in ms (default 5000)
     :safe-integers?    read INTEGER columns as BigInt (default false; see
                        bigint->number)
     :foreign-keys?     PRAGMA foreign_keys (default true; set explicitly)
     :journal-mode      :wal (default for file databases) :delete :truncate
                        :persist :memory :off; ignored for :memory: databases
                        (SQLite answers \"memory\" and cannot switch)
     :require-journal-mode?  when true (default) raise
                        :sqlite-cljs/config-error {:reason :journal-mode-unavailable}
                        if SQLite could not switch (WAL on a network share)
     :synchronous       :off :normal :full :extra (or 0-3); when omitted the
                        driver default stays (NORMAL for a WAL database
                        opened by a fresh connection, FULL otherwise);
                        the effective value is reported in :effective
     :trusted-schema?   PRAGMA trusted_schema (default false per sqlite.org)
     :cache-size        statement cache entries (default 128)

   This constructor is synchronous (it returns the value, not a promise):
   opening is the one operation with no asynchronous counterpart to swap
   in later. Errors: :sqlite-cljs/config-error for bad options or an
   unopenable directory, :sqlite-cljs/sqlite-error (SQLITE_CANTOPEN,
   SQLITE_READONLY ...) from SQLite."
  ([] (db {:memory? true}))
  ([path-or-opts]
   (let [opts (cond
                (map? path-or-opts) path-or-opts
                (string? path-or-opts) {:path path-or-opts}
                (nil? path-or-opts) {:memory? true}
                :else (throw (util/config-error (str "db: expected a path string or an options map, got " (pr-str path-or-opts))
                                                {:reason :bad-options :value path-or-opts})))
         {:keys [path memory? readonly? timeout-ms safe-integers? foreign-keys? journal-mode
                 require-journal-mode? synchronous trusted-schema? cache-size]
          :or {timeout-ms 5000 foreign-keys? true require-journal-mode? true
               trusted-schema? false cache-size default-cache-size}} opts
         memory? (boolean (or memory? (nil? path) (= path ":memory:")))
         path (if memory? ":memory:" path)]
     (when-not (string? path)
       (throw (util/config-error "db: :path must be a string" {:reason :bad-options :value path})))
     (when (and memory? readonly?)
       (throw (util/config-error "db: an in-memory database cannot be read-only"
                                 {:reason :bad-options :value opts})))
     (when-not (and (integer? timeout-ms) (>= timeout-ms 0))
       (throw (util/config-error "db: :timeout-ms must be a non-negative integer"
                                 {:reason :bad-options :value timeout-ms})))
     (when (and (some? journal-mode) (not (contains? journal-modes journal-mode)))
       (throw (util/config-error (str "db: unknown :journal-mode " (pr-str journal-mode))
                                 {:reason :bad-options :value journal-mode})))
     (when (and (some? synchronous)
                (not (or (contains? synchronous-modes synchronous)
                         (contains? #{0 1 2 3} synchronous))))
       (throw (util/config-error (str "db: unknown :synchronous " (pr-str synchronous))
                                 {:reason :bad-options :value synchronous})))
     (let [c (open-connection path {:readonly? readonly?
                                    :file-must-exist? (:file-must-exist? opts)
                                    :timeout-ms timeout-ms})
           d {:db c :path path :memory? memory? :open-opts opts
              :cache (js/Map.) :cache-size cache-size}]
       (try
         (when safe-integers? (js-invoke c "defaultSafeIntegers" true))
         (pragma* d :foreign-keys {:value (boolean foreign-keys?)})
         (pragma* d :trusted-schema {:value (boolean trusted-schema?)})
         (let [wanted (cond memory? nil
                            (some? journal-mode) journal-mode
                            readonly? nil
                            :else :wal)
               mode (if wanted
                      (pragma* d :journal-mode {:value wanted})
                      (pragma* d :journal-mode))]
           (when (and wanted require-journal-mode?
                      (not= (str/lower-case (str mode)) (name wanted)))
             (js-invoke c "close")
             (throw (util/config-error
                     (str "db: could not switch journal_mode to " (name wanted)
                          " (SQLite kept " (pr-str mode) "); WAL does not work on network filesystems")
                     {:reason :journal-mode-unavailable :wanted wanted :actual mode :path path})))
           (when (some? synchronous)
             (pragma* d :synchronous {:value synchronous}))
           (assoc d :effective
                  {:journal-mode (str/lower-case (str mode))
                   :synchronous (pragma* d :synchronous)
                   :foreign-keys? (= 1 (pragma* d :foreign-keys))
                   :busy-timeout-ms (:timeout (first (pragma* d :busy-timeout {:rows? true})))
                   :trusted-schema? (= 1 (pragma* d :trusted-schema))
                   :safe-integers? (boolean safe-integers?)}))
         (catch :default e
           (when (unchecked-get c "open") (js-invoke c "close"))
           (throw e)))))))

(defn open?
  "True while the connection is open. Synchronous predicate."
  [d]
  (boolean (some-> (:db d) (unchecked-get "open"))))

(defn in-transaction?
  "True while a transaction (transact! or a manual BEGIN) is active.
   Synchronous predicate."
  [d]
  (boolean (some-> (:db d) (unchecked-get "inTransaction"))))

(defn close!
  "Closes the connection (idempotent: closing twice is fine) and clears the
   statement cache. Resolves to nil. opts :optimize? true runs PRAGMA
   optimize first (sqlite.org recommends it for long-lived connections)."
  ([d] (close! d {}))
  ([d {:keys [optimize?]}]
   (try
     (let [c (conn d)]
       (when (and optimize? (unchecked-get c "open"))
         (pragma* d :optimize))
       (clear-cache! d)
       (js-invoke c "close")
       (p/resolved nil))
     (catch :default e (p/rejected (wrap-error e nil))))))

;; ---------------------------------------------------------------------------
;; Statements (synchronous internals used by sqlite-cljs.query)
;; ---------------------------------------------------------------------------

(defn prepare*
  "Prepared statement for `sql` (one statement only), from the connection's
   LRU cache. Internal."
  [d sql]
  (when-not (string? sql)
    (throw (util/argument-error (str "sql must be a string, got " (pr-str sql)) {:reason :not-sql :value sql})))
  (let [m (:cache d)]
    (or (cache-get m sql)
        (let [st (try (js-invoke (conn d) "prepare" sql)
                      (catch :default e (throw (wrap-error e sql))))]
          (cache-put m sql st (or (:cache-size d) default-cache-size))))))

(defn- with-safe-integers [d st opts]
  (let [safe? (if (contains? opts :safe-integers?)
                (boolean (:safe-integers? opts))
                (boolean (:safe-integers? (:effective d))))]
    (js-invoke st "safeIntegers" safe?)
    st))

(defn reader?*
  "True when the statement returns rows (SELECT, or a write with RETURNING).
   Internal."
  [d sql]
  (boolean (unchecked-get (prepare* d sql) "reader")))

(defn run*
  "Executes a non-reader statement; returns {:changes n :last-insert-rowid n}.
   Internal, synchronous, throws sqlite-cljs errors."
  ([d sql params] (run* d sql params {}))
  ([d sql params opts]
   (let [st (with-safe-integers d (prepare* d sql) opts)
         arr (->params params)]
     (try
       (let [r (js-invoke st "run" arr)]
         {:changes (unchecked-get r "changes")
          :last-insert-rowid (unchecked-get r "lastInsertRowid")})
       (catch :default e (throw (wrap-error e sql)))))))

(defn all*
  "Executes a reader statement; returns a vector of kebab row maps (opts
   :safe-integers?, :coerce). Internal, synchronous."
  ([d sql params] (all* d sql params {}))
  ([d sql params opts]
   (let [st (with-safe-integers d (prepare* d sql) opts)
         arr (->params params)
         rows (try (util/rows->clj (js-invoke st "all" arr))
                   (catch :default e (throw (wrap-error e sql))))]
     (if-let [c (:coerce opts)]
       (mapv #(coerce-row c %) rows)
       rows))))

(defn get*
  "First row of a reader statement or nil. Internal, synchronous."
  ([d sql params] (get* d sql params {}))
  ([d sql params opts]
   (let [st (with-safe-integers d (prepare* d sql) opts)
         arr (->params params)
         row (try (util/row->clj (js-invoke st "get" arr))
                  (catch :default e (throw (wrap-error e sql))))]
     (if-let [c (:coerce opts)]
       (some->> row (coerce-row c))
       row))))

(defn columns*
  "Result column metadata of a reader statement: vector of
   {:name :column :table :database :type}. Internal, synchronous."
  [d sql]
  (let [st (prepare* d sql)]
    (try (util/rows->clj (js-invoke st "columns"))
         (catch :default e (throw (wrap-error e sql))))))

(defn exec*
  "Runs one or more SQL statements with NO parameters (driver exec) and
   clears the statement cache (the text may contain DDL). Internal,
   synchronous."
  [d sql]
  (when-not (string? sql)
    (throw (util/argument-error "exec expects a SQL string" {:reason :not-sql :value sql})))
  (try
    (js-invoke (conn d) "exec" sql)
    (clear-cache! d)
    nil
    (catch :default e (throw (wrap-error e sql)))))

(def transaction-modes
  "Accepted transact! modes."
  #{:deferred :immediate :exclusive})

(defn transaction*
  "Runs (f d) inside the driver's transaction wrapper (BEGIN <mode> ..
   COMMIT, ROLLBACK + rethrow on any throw). Nested calls (or a manual
   BEGIN in progress) are rejected with :sqlite-cljs/argument-error
   {:reason :nested-transaction}. `f` must be synchronous: a returned
   promise is an argument-error (the transaction would have committed
   before the first continuation). Internal, synchronous."
  [d mode f]
  (when-not (contains? transaction-modes mode)
    (throw (util/argument-error (str "unknown transaction mode " (pr-str mode)) {:reason :bad-mode :value mode})))
  (let [c (conn d)]
    (when (unchecked-get c "inTransaction")
      (throw (util/argument-error "nested transact! is not allowed (a transaction is already active on this connection)"
                                  {:reason :nested-transaction})))
    (let [wrapped (js-invoke c "transaction" (fn []
                                    (let [r (f d)]
                                      (when (p/promise? r)
                                        (throw (util/argument-error
                                                "transact! body returned a promise; bodies run synchronously — deref settled promises inside (@(execute! tx q)) and return plain values"
                                                {:reason :async-body})))
                                      r)))]
      (try
        (case mode
          :deferred (js-invoke wrapped "deferred")
          :immediate (js-invoke wrapped "immediate")
          :exclusive (js-invoke wrapped "exclusive"))
        (catch :default e (throw (wrap-error e nil)))))))

;; ---------------------------------------------------------------------------
;; Promise API: pragmas, versions, UDFs
;; ---------------------------------------------------------------------------

(defn- settle [thunk]
  (try (p/resolved (thunk))
       (catch :default e (p/rejected (wrap-error e nil)))))

(defn pragma
  "Reads a PRAGMA: (pragma d :journal-mode) resolves to \"wal\" (the first
   column of the first row). (pragma d :table-info {:arg :users :rows? true})
   resolves to a vector of kebab row maps."
  ([d name-kw] (pragma d name-kw {}))
  ([d name-kw opts] (settle #(pragma* d name-kw (dissoc opts :value)))))

(defn pragma!
  "Sets a PRAGMA and resolves to the re-read value: (pragma! d :user-version 7)
   => 7; (pragma! d :journal-mode :wal) => \"wal\" (or the mode SQLite kept).
   Values are validated as bare words/integers/booleans/keywords; never
   user data. NOTE: PRAGMA foreign_keys is a no-op inside a transaction."
  [d name-kw value]
  (settle (fn []
            (pragma* d name-kw {:value value})
            (pragma* d name-kw))))

(defn user-version
  "Resolves to PRAGMA user_version (the migrate! fingerprint)."
  [d]
  (pragma d :user-version))

(defn sqlite-version
  "Resolves to the SQLite library version string of the connection
   (\"3.53.4\")."
  [d]
  (settle #(:v (get* d "SELECT sqlite_version() AS v" []))))

(defn sqlite-version*
  "Synchronous sqlite-version. Internal (used by ddl gating)."
  [d]
  (:v (get* d "SELECT sqlite_version() AS v" [])))

(defn library-version
  "Resolves to the wrapped driver's package version string (\"13.0.3\").
   The db argument is accepted for API symmetry and not used."
  [_d]
  (settle #(deref driver-version)))

(defn driver-info
  "Resolves to {:driver \"better-sqlite3\" :driver-version .. :sqlite-version ..
   :path .. :memory? .. :readonly? .. :effective {..}}."
  [d]
  (settle (fn []
            (let [c (conn d)]
              {:driver driver-name
               :driver-version @driver-version
               :sqlite-version (sqlite-version* d)
               :path (:path d)
               :memory? (boolean (unchecked-get c "memory"))
               :readonly? (boolean (unchecked-get c "readonly"))
               :open? (boolean (unchecked-get c "open"))
               :effective (:effective d)}))))

(defn define-function!
  "Registers a scalar SQL function `name` (string, [a-z_][a-z0-9_]*) backed
   by `f`; arguments arrive as driver values (numbers/strings/nil/Buffer,
   BigInt with :safe-integers?), the return value must be bindable. opts
   :deterministic? :direct-only? :varargs?. Resolves to nil."
  ([d fname f] (define-function! d fname f {}))
  ([d fname f {:keys [deterministic? direct-only? varargs?]}]
   (settle (fn []
             (when-not (and (string? fname) (re-matches #"[a-z_][a-z0-9_]*" fname))
               (throw (util/argument-error (str "invalid SQL function name " (pr-str fname)) {:value fname})))
             (js-invoke (conn d) "function" fname
                        #js {:deterministic (boolean deterministic?)
                             :directOnly (boolean direct-only?)
                             :varargs (boolean varargs?)}
                        f)
             nil))))
