(ns sqlite-cljs.util
  "Small shared helpers: keyword case translation (kebab keywords <-> snake
   SQL names), strict SQL identifier quoting, row conversion and error
   normalization. No I/O and no driver access: the only namespace that
   touches the native driver is sqlite-cljs.core."
  (:require [clojure.string :as str]))

;; ---------------------------------------------------------------------------
;; kebab-case <-> snake_case (kebab keywords in, snake names out)
;; ---------------------------------------------------------------------------

(defn kebab->snake
  "\"user-name\" -> \"user_name\". Keywords render as their name (the
   namespace is dropped; see sql-qualified for qualified references); strings
   pass through untouched (they are assumed to be exact SQL names already)."
  [k]
  (cond
    (keyword? k) (str/replace (name k) "-" "_")
    (symbol? k) (str/replace (name k) "-" "_")
    (string? k) k
    :else (str k)))

(defn snake->kebab
  "\"user_name\" -> :user-name. Works on strings and keywords, returns a
   keyword. Namespaced keywords are returned unchanged."
  [k]
  (if (and (keyword? k) (namespace k))
    k
    (keyword (str/replace (name k) "_" "-"))))

(defn transform-keys
  "Recursively applies `f` to every map key in `x` (maps inside vectors/lists
   included). Keys listed in `passthrough` (a set) keep their subtree
   untouched."
  ([f x] (transform-keys f #{} x))
  ([f passthrough x]
   (cond
     (map? x) (into (empty x)
                    (map (fn [[k v]]
                           (if (contains? passthrough k)
                             [(f k) v]
                             [(f k) (transform-keys f passthrough v)])))
                    x)
     (sequential? x) (into (empty x) (map #(transform-keys f passthrough %)) x)
     :else x)))

;; ---------------------------------------------------------------------------
;; SQL identifiers. HARD RULE: identifiers are always quoted
;; with double quotes and embedded double quotes are doubled; values NEVER go
;; through this path (they are bound parameters).
;; ---------------------------------------------------------------------------

(defn ident-name
  "The unquoted SQL name of an identifier designator: keywords/symbols are
   rendered kebab->snake (namespace dropped), strings are exact. Throws
   :sqlite-cljs/argument-error for anything else, for the empty string and
   for names containing a NUL character (an embedded NUL would truncate the
  [x]
  (let [s (cond
            (or (keyword? x) (symbol? x)) (kebab->snake x)
            (string? x) x
            :else (throw (ex-info (str "sqlite-cljs: not an identifier: " (pr-str x))
                                  {:type :sqlite-cljs/argument-error :value x})))]
    (when (= "" s)
      (throw (ex-info "sqlite-cljs: empty identifier"
                      {:type :sqlite-cljs/argument-error :value x})))
    (when (str/includes? s "\u0000")
      (throw (ex-info "sqlite-cljs: identifier contains a NUL character"
                      {:type :sqlite-cljs/argument-error :value x})))
    s))

(defn sql-ident
  "Quotes ONE identifier (table, column, index, view, constraint or schema
   name) as a double-quoted SQL identifier: :user-name -> \"user_name\",
   \"we\\\"ird\" -> \"we\"\"ird\". Never splits on dots or namespaces: the
   whole name is one identifier. Any string is a valid identifier once
   quoted this way, so injection attempts stay inert names."
  [x]
  (str "\"" (str/replace (ident-name x) "\"" "\"\"") "\""))

(defn sql-qualified
  "Quotes a possibly qualified column/table reference. A keyword namespace
   is the qualifier (:users/id -> \"users\".\"id\"); both namespace and name
   are additionally split on dots (:users.id and :main.users/id work, honeysql
   style) and every part is rendered kebab->snake and quoted separately. A
   trailing * is rendered bare (:users/* -> \"users\".*, :* -> *). Strings
   are treated as ONE exact identifier (no splitting, no case translation),
   so a column that really contains a dot is reachable by passing a string."
  [x]
  (cond
    (string? x) (sql-ident x)
    (or (keyword? x) (symbol? x))
    (let [parts (cond-> [] (namespace x) (into (str/split (namespace x) #"\.")))
          parts (into parts (str/split (name x) #"\."))]
      (str/join "." (map #(if (= "*" %) "*" (sql-ident (str/replace % "-" "_"))) parts)))
    :else (sql-ident x)))

;; ---------------------------------------------------------------------------
;; JS interop conversions (rows from the driver, values to the driver)
;; ---------------------------------------------------------------------------

(defn row->clj
  "A driver row object -> map with kebab keyword keys. Uses Object.keys so it
   also works for null-prototype objects (which js->clj leaves untouched).
   Values are not converted: numbers, strings, nil, BigInt and Buffers pass
   through as the driver returned them."
  [o]
  (when (some? o)
    (persistent!
     (reduce (fn [m k] (assoc! m (snake->kebab k) (unchecked-get o k)))
             (transient {})
             (js/Object.keys o)))))

(defn rows->clj
  "A driver row array -> vector of row->clj maps."
  [a]
  (if (nil? a) [] (mapv row->clj (array-seq a))))

;; ---------------------------------------------------------------------------
;; Errors
;; ---------------------------------------------------------------------------

(def error-types
  "Every :type a sqlite-cljs ex-info can carry."
  #{:sqlite-cljs/sqlite-error :sqlite-cljs/argument-error :sqlite-cljs/dsl-error
    :sqlite-cljs/config-error :sqlite-cljs/migration-error})

(defn sqlite-error
  "ex-info wrapping an error thrown by the driver/SQLite. Data:
   {:type :sqlite-cljs/sqlite-error :code \"SQLITE_CONSTRAINT_UNIQUE\" (the
   extended result code string when the driver provides one) :message ..
   :sql .. :cause <original error>}."
  [cause & [{:keys [sql]}]]
  (let [message (or (some-> cause .-message) (str cause))
        code (some-> cause .-code)]
    (ex-info (str "SQLite error" (when code (str " " code)) ": " message)
             (cond-> {:type :sqlite-cljs/sqlite-error
                      :message message
                      :cause cause}
               code (assoc :code code)
               sql (assoc :sql sql)))))

(defn argument-error
  "ex-info for an invalid argument detected before any SQL runs."
  [message & [data]]
  (ex-info (str "sqlite-cljs: " message)
           (merge {:type :sqlite-cljs/argument-error} data)))

(defn dsl-error
  "ex-info for a DSL form the renderer refuses (unknown clause, malformed
   column definition, value where an identifier is required, ...). Data:
   {:type :sqlite-cljs/dsl-error :form <offending form> ..}."
  [message form & [data]]
  (ex-info (str "sqlite-cljs DSL: " message)
           (merge {:type :sqlite-cljs/dsl-error :form form} data)))

(defn config-error
  "ex-info for a configuration problem (driver missing, bad open options)."
  [message & [data]]
  (ex-info (str "sqlite-cljs: " message)
           (merge {:type :sqlite-cljs/config-error} data)))

(defn migration-error
  "ex-info for a migration that cannot be applied (drift, a database newer
   than the definitions, a statement SQLite refuses)."
  [message & [data]]
  (ex-info (str "sqlite-cljs migration: " message)
           (merge {:type :sqlite-cljs/migration-error} data)))

(defn error-data
  "Returns the structured data of a sqlite-cljs error, or nil when `e` is
   not one."
  [e]
  (when-let [d (ex-data e)]
    (when (contains? error-types (:type d))
      d)))

(defn error-type
  "One of the sqlite-cljs error :type keywords (see error-types) or nil."
  [e]
  (:type (error-data e)))

(defn sqlite-code
  "The SQLite extended result code string of a sqlite-cljs sqlite-error
   (\"SQLITE_BUSY\", \"SQLITE_CONSTRAINT_UNIQUE\", ...) or nil."
  [e]
  (when (= :sqlite-cljs/sqlite-error (error-type e))
    (:code (error-data e))))

(defn constraint-error?
  "True when `e` is a sqlite-error whose code starts with SQLITE_CONSTRAINT."
  [e]
  (boolean (some-> (sqlite-code e) (str/starts-with? "SQLITE_CONSTRAINT"))))

(defn busy-error?
  "True when `e` is a sqlite-error whose code starts with SQLITE_BUSY."
  [e]
  (boolean (some-> (sqlite-code e) (str/starts-with? "SQLITE_BUSY"))))

;; ---------------------------------------------------------------------------
;; SQLite version comparison (feature gates)
;; ---------------------------------------------------------------------------

(defn version-vec
  "\"3.53.4\" -> [3 53 4]. Missing components are 0; non-numeric input
   throws :sqlite-cljs/argument-error."
  [s]
  (let [parts (str/split (str s) #"\.")
        nums (mapv #(js/parseInt % 10) parts)]
    (when (or (empty? parts) (some js/isNaN nums))
      (throw (argument-error (str "not a version string: " (pr-str s)) {:value s})))
    (into nums (repeat (- 3 (count nums)) 0))))

(defn version>=
  "True when SQLite version string `have` is at least version string `need`
   (\"3.53.4\" >= \"3.35.0\"). A nil `have` means \"unknown\" and counts as
   satisfied: callers gate only when they know the connection's version."
  [have need]
  (or (nil? have)
      (>= (compare (version-vec have) (version-vec need)) 0)))

(defn sql-string-literal
  "Renders a string as a single-quoted SQL literal with ' doubled (DDL
   literal mode only). Values in queries are NEVER
   rendered this way: they are bound parameters. NUL characters are
   rejected (SQLite string functions stop at a NUL)."
  [s]
  (when (str/includes? s "\u0000")
    (throw (argument-error "string literal contains a NUL character" {:value s})))
  (str "'" (str/replace s "'" "''") "'"))
