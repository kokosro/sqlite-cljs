(ns sqlite-cljs.dsl
  "Pure query renderer: honeysql-inspired data -> {:sql \"...\" :params [...]}
   in SQLite dialect. No I/O, no driver access.

   Conventions:
   * maps are statements ({:select .. :from ..}, {:insert-into ..}, {:update ..},
     {:delete-from ..}, {:union [..]}); a map in an expression position is a
     subquery.
   * vectors starting with a keyword are operator/function calls:
     [:= :id 1], [:and ..], [:count :*], [:lower :name], [:raw \"sql\" & params].
   * keywords are identifiers, rendered kebab->snake and ALWAYS quoted:
     :user-name -> \"user_name\", :u/name -> \"u\".\"name\", :* -> *.
   * strings in expression positions are VALUES (bound parameters); strings
     in identifier positions (:from, :columns, select items, :set keys, ..)
     are exact identifiers.
   * every value becomes a `?` parameter, in left-to-right order. Values
     are never interpolated into SQL. Booleans bind as 1/0 (sqlite-cljs.core).
   * nil is NULL: [:= :x nil] renders \"x\" IS NULL.

   Select items: :col | \"Exact\" | [expr :alias] | [expr] | [:as expr :alias].
   A two-element vector in a select list is ALWAYS `expr AS alias`, so a bare
   function call without alias must be wrapped ([[:lower :name]]) or use
   [:as ..] — the same rule as honeysql. [:count :*] is the one exception
   (\"*\" cannot be an alias).

   Duplicate result column names in one select are a dsl-error (the driver
   collapses them into one map key): add aliases.

   Joins: {:joins [[:left-join [:orgs :o] [:= :o/id :u/org-id]] ...]} renders
   in the given order; the honeysql clause keys :join/:inner-join/:left-join/
   :right-join/:full-join/:cross-join ([table cond table cond ...]) are also
   accepted and rendered after :joins in that fixed order. A condition is an
   expression (ON ..) or [:using :col ..].

   IN lists: [:in :id coll] with more than :in-json-threshold (500) values
   renders \"id\" IN (SELECT value FROM json_each(?)) with ONE JSON-array
   parameter (numbers/strings/nil/booleans only); [:in :id [:json-each coll]]
   forces that path. Empty coll renders 0=1 (:not-in -> 1=1).

   Render options: {:mode :params (default) | :literal, :in-json-threshold n,
   :sqlite-version \"3.53.4\", :max-params 32766}. Literal mode is for DDL
   expressions only (CHECK/DEFAULT/views/partial indexes cannot take
   parameters) and inlines a restricted literal vocabulary."
  (:require [clojure.string :as str]
            [sqlite-cljs.util :as util]))

(def default-in-json-threshold
  "Above this many values, [:in x coll] switches to the json_each path."
  500)

(def default-max-params
  "SQLITE_MAX_VARIABLE_NUMBER of the bundled SQLite."
  32766)

(def default-opts
  {:mode :params
   :in-json-threshold default-in-json-threshold
   :max-params default-max-params
   :sqlite-version nil})

(def type-names
  "Column / CAST type vocabulary: keyword -> SQL type."
  {:integer "INTEGER" :int "INT" :real "REAL" :text "TEXT" :blob "BLOB"
   :any "ANY" :numeric "NUMERIC"})

;; ---------------------------------------------------------------------------
;; fragments
;; ---------------------------------------------------------------------------

(defn- frag
  ([sql] {:sql sql :params []})
  ([sql params] {:sql sql :params (vec params)}))

(defn- cat-frags
  "Joins fragments with `sep`, concatenating params in order."
  [sep frags]
  {:sql (str/join sep (map :sql frags))
   :params (into [] (mapcat :params) frags)})

(defn- paren [f] (update f :sql #(str "(" % ")")))

(defn- prefix [s f] (update f :sql #(str s %)))

(defn- ident-pos?
  "True for values accepted in identifier positions (keyword or exact string)."
  [x]
  (or (keyword? x) (string? x)))

(defn- ident
  "Identifier position: keyword (possibly qualified) or exact string."
  [x form]
  (when-not (ident-pos? x)
    (throw (util/dsl-error (str "expected an identifier (keyword or string), got " (pr-str x)) form)))
  (util/sql-qualified x))

(defn- bigint? [v] (= "bigint" (goog/typeOf v)))

(defn- blob? [v] (or (instance? js/Uint8Array v) (instance? js/ArrayBuffer v)))

(defn- bindable? [v]
  (or (nil? v) (boolean? v) (number? v) (string? v) (bigint? v) (blob? v)))

(defn- literal-sql
  "Literal mode value."
  [v form]
  (cond
    (nil? v) "NULL"
    (true? v) "1"
    (false? v) "0"
    (number? v)
    (cond
      (or (js/isNaN v) (not (js/isFinite v)))
      (throw (util/dsl-error "NaN/Infinity cannot be a literal" form))
      (and (js/Number.isInteger v) (> (js/Math.abs v) js/Number.MAX_SAFE_INTEGER))
      (throw (util/dsl-error "integer literal beyond 2^53-1" form))
      :else (str v))
    (string? v) (util/sql-string-literal v)
    (bigint? v) (str v)
    :else (throw (util/dsl-error (str "value cannot be rendered as a DDL literal: " (pr-str v)) form))))

(defn- value
  "A value in expression position."
  [v ctx form]
  (when-not (bindable? v)
    (throw (util/dsl-error
            (str "not a bindable value: " (pr-str v)
                 (cond (keyword? v) " (keywords are identifiers; use a string for an enum-like value)"
                       (coll? v) " (collections are never auto-serialised; wrap [:param v] or serialise to a string)"
                       (instance? js/Date v) " (store epoch milliseconds: (.getTime d))"
                       :else ""))
            form {:value v})))
  (if (= :literal (:mode ctx))
    (frag (literal-sql v form))
    (frag "?" [v])))

;; ---------------------------------------------------------------------------
;; expressions
;; ---------------------------------------------------------------------------

(declare expr statement-sql select-sql order-item)

(defn- paren-call [fname argfrag]
  (update argfrag :sql #(str fname "(" % ")")))

(defn- subquery? [x] (map? x))

(defn- sql-fn-name
  "Function names render unquoted: kebab->snake, [a-z_][a-z0-9_]* only."
  [k form]
  (let [s (util/kebab->snake k)]
    (when-not (re-matches #"[a-z_][a-z0-9_]*" s)
      (throw (util/dsl-error (str "invalid SQL function name " (pr-str k)) form)))
    s))

(def ^:private comparison-ops
  {:= "=" :not= "<>" :<> "<>" :!= "<>" :< "<" :<= "<=" :> ">" :>= ">="})

(def ^:private infix-ops
  {:+ " + " :- " - " :* " * " :/ " / " :% " % " :|| " || " :-> " -> " :->> " ->> "
   :& " & " :| " | " :<< " << " :>> " >> "})

(def ^:private like-ops
  {:like "LIKE" :not-like "NOT LIKE" :glob "GLOB" :not-glob "NOT GLOB"
   :regexp "REGEXP" :not-regexp "NOT REGEXP" :match "MATCH"})

(defn- json-array-param
  "Serialises a collection for the json_each path (numbers, strings,
   booleans as 1/0, nil as null)."
  [coll form]
  (js/JSON.stringify
   (clj->js
    (mapv (fn [v]
            (cond
              (nil? v) nil
              (boolean? v) (if v 1 0)
              (number? v) (if (and (js/isFinite v) (not (js/isNaN v)))
                            v
                            (throw (util/dsl-error "NaN/Infinity in an IN list" form)))
              (string? v) v
              :else (throw (util/dsl-error
                            (str "json_each IN lists accept numbers, strings, booleans and nil only; got " (pr-str v))
                            form {:value v}))))
          coll))))

(defn- json-each-frag [coll ctx form]
  (when (= :literal (:mode ctx))
    (throw (util/dsl-error "json_each IN lists need a parameter; not available in DDL literal mode" form)))
  (frag "(SELECT value FROM json_each(?))" [(json-array-param coll form)]))

(defn- json-each-form? [x]
  (and (vector? x) (= :json-each (first x)) (sequential? (second x)) (= 2 (count x))))

(defn- in-expr [negate? form ctx]
  (let [[_ x coll] form]
    (when-not (= 3 (count form))
      (throw (util/dsl-error "[:in x coll] takes exactly two arguments" form)))
    (let [lhs (expr x ctx)
          kw (if negate? " NOT IN " " IN ")]
      (cond
        (subquery? coll) (cat-frags "" [lhs (frag kw) (paren (statement-sql coll ctx))])
        (json-each-form? coll) (cat-frags "" [lhs (frag kw) (json-each-frag (second coll) ctx form)])
        (sequential? coll)
        (cond
          (empty? coll) (frag (if negate? "1=1" "0=1"))
          (> (count coll) (or (:in-json-threshold ctx) default-in-json-threshold))
          (cat-frags "" [lhs (frag kw) (json-each-frag coll ctx form)])
          :else (cat-frags "" [lhs (frag kw) (paren (cat-frags ", " (map #(expr % ctx) coll)))]))
        :else (throw (util/dsl-error "[:in x coll]: coll must be a vector, a subquery map or [:json-each coll]" form))))))

(defn- like-opts [form]
  (let [o (nth form 3 nil)]
    (when (some? o)
      (when-not (and (map? o) (string? (:escape o)) (= 1 (count (:escape o))))
        (throw (util/dsl-error "LIKE options must be {:escape \"one-char\"}" form)))
      o)))

(defn- case-expr [form ctx]
  (let [parts (rest form)
        [pairs else] (let [v (vec parts)]
                       (if (and (>= (count v) 2) (= :else (nth v (- (count v) 2))))
                         [(subvec v 0 (- (count v) 2)) [(peek v)]]
                         [v nil]))]
    (when (empty? pairs)
      (throw (util/dsl-error "[:case [cond val] ... :else v] needs at least one [cond val] pair" form)))
    (cat-frags
     " "
     (concat [(frag "CASE")]
             (mapcat (fn [pr]
                       (when-not (and (vector? pr) (= 2 (count pr)))
                         (throw (util/dsl-error "each :case branch is a [condition value] pair" form)))
                       [(frag "WHEN") (expr (first pr) ctx) (frag "THEN") (expr (second pr) ctx)])
                     pairs)
             (when else [(frag "ELSE") (expr (first else) ctx)])
             [(frag "END")]))))

(defn- window-spec [spec ctx form]
  (when-not (map? spec)
    (throw (util/dsl-error "[:over fn-call {:partition-by [..] :order-by [..]}]" form)))
  (let [unknown (remove #{:partition-by :order-by} (keys spec))]
    (when (seq unknown)
      (throw (util/dsl-error (str "unknown window clause(s) " (pr-str (vec unknown)) " (frames are not supported; use [:raw ..])") form))))
  (paren
   (cat-frags " "
              (remove nil?
                      [(when-let [pb (seq (:partition-by spec))]
                         (prefix "PARTITION BY " (cat-frags ", " (map #(expr % ctx) pb))))
                       (when-let [ob (seq (:order-by spec))]
                         (prefix "ORDER BY " (cat-frags ", " (map #(order-item % ctx) ob))))]))))

(def ^:private order-flags #{:asc :desc :nulls-first :nulls-last})

(defn- order-flag? [f]
  (or (contains? order-flags f)
      (and (vector? f) (= 2 (count f)) (= :collate (first f)) (ident-pos? (second f)))))

(defn- order-item
  "ORDER BY item: expr | [expr & flags], flags in #{:asc :desc :nulls-first
   :nulls-last} or [:collate :nocase]. The first element of a vector item
   is the expression (a keyword is a column), so an expression call must be
   wrapped: [[:lower :email] :asc]."
  [item ctx]
  (if (vector? item)
    (let [[e & flags] item
          bad (remove order-flag? flags)]
      (when (empty? item) (throw (util/dsl-error "empty ORDER BY item" item)))
      (when (seq bad)
        (throw (util/dsl-error (str "unknown ORDER BY flag(s) " (pr-str (vec bad)) " (wrap expression calls: [[:lower :x] :asc])") item)))
      (cat-frags " "
                 (cons (expr e ctx)
                       (map (fn [f]
                              (case f
                                :asc (frag "ASC") :desc (frag "DESC")
                                :nulls-first (frag "NULLS FIRST") :nulls-last (frag "NULLS LAST")
                                (frag (str "COLLATE " (str/upper-case (util/kebab->snake (second f)))))))
                            flags))))
    (expr item ctx)))

(defn- fn-call [form ctx]
  (let [[f & args] form
        fname (sql-fn-name f form)]
    (cond
      (and (= 1 (count args)) (= :* (first args)))
      (frag (str fname "(*)"))
      (some #(= :* %) args)
      (throw (util/dsl-error ":* is only allowed as the sole function argument" form))
      :else
      (paren-call fname (cat-frags ", " (map #(expr % ctx) args))))))

(defn- binary [op form ctx]
  (when-not (= 3 (count form))
    (throw (util/dsl-error (str "[" op " a b] takes exactly two arguments") form)))
  (cat-frags (str " " op " ") [(expr (nth form 1) ctx) (expr (nth form 2) ctx)]))

(defn expr
  "Renders one expression -> {:sql :params}. See the namespace docstring for
   the grammar."
  [e ctx]
  (cond
    (nil? e) (if (= :literal (:mode ctx)) (frag "NULL") (frag "?" [nil]))
    (keyword? e) (frag (util/sql-qualified e))
    (subquery? e) (paren (statement-sql e ctx))
    (vector? e)
    (let [op (first e)
          form e]
      (when-not (keyword? op)
        (throw (util/dsl-error (str "expression vectors start with an operator keyword, got " (pr-str op)) form)))
      (cond
        (= :param op)
        (do (when-not (= 2 (count form)) (throw (util/dsl-error "[:param v] takes one value" form)))
            (value (second form) ctx form))

        (= :raw op)
        (let [[_ sql & params] form]
          (when-not (string? sql) (throw (util/dsl-error "[:raw \"sql\" & params]: the SQL must be a string" form)))
          (when (and (seq params) (= :literal (:mode ctx)))
            (throw (util/dsl-error "[:raw ..] with parameters is not allowed in DDL literal mode" form)))
          (doseq [p params]
            (when-not (bindable? p)
              (throw (util/dsl-error (str "[:raw ..] parameter is not bindable: " (pr-str p)) form))))
          (frag sql params))

        (contains? comparison-ops op)
        (let [[_ a b] form
              sym (comparison-ops op)]
          (when-not (= 3 (count form))
            (throw (util/dsl-error (str "[" op " a b] takes exactly two arguments") form)))
          (cond
            (and (nil? b) (contains? #{:= :not= :<> :!=} op))
            (cat-frags "" [(expr a ctx) (frag (if (= := op) " IS NULL" " IS NOT NULL"))])
            (and (nil? a) (contains? #{:= :not= :<> :!=} op))
            (cat-frags "" [(expr b ctx) (frag (if (= := op) " IS NULL" " IS NOT NULL"))])
            :else (cat-frags (str " " sym " ") [(expr a ctx) (expr b ctx)])))

        (contains? #{:and :or} op)
        (let [args (rest form)]
          (cond
            (empty? args) (frag (if (= :and op) "1=1" "0=1"))
            (= 1 (count args)) (expr (first args) ctx)
            :else (paren (cat-frags (if (= :and op) " AND " " OR ") (map #(expr % ctx) args)))))

        (= :not op)
        (do (when-not (= 2 (count form)) (throw (util/dsl-error "[:not x] takes one argument" form)))
            (prefix "NOT " (paren (expr (second form) ctx))))

        (= :in op) (in-expr false form ctx)
        (= :not-in op) (in-expr true form ctx)

        (contains? #{:is-null :not-null :is-not-null} op)
        (do (when-not (= 2 (count form)) (throw (util/dsl-error (str "[" op " x] takes one argument") form)))
            (cat-frags "" [(expr (second form) ctx) (frag (if (= :is-null op) " IS NULL" " IS NOT NULL"))]))

        (contains? #{:is :is-not :is-distinct-from :is-not-distinct-from} op)
        (binary (case op :is "IS" :is-not "IS NOT" :is-distinct-from "IS DISTINCT FROM" "IS NOT DISTINCT FROM") form ctx)

        (contains? like-ops op)
        (let [[_ x pat] form
              o (like-opts form)]
          (when-not (<= 3 (count form) 4)
            (throw (util/dsl-error (str "[" op " x pattern {:escape ..}?]") form)))
          (cat-frags "" (remove nil?
                                [(expr x ctx) (frag (str " " (like-ops op) " ")) (expr pat ctx)
                                 (when o (cat-frags "" [(frag " ESCAPE ") (value (:escape o) ctx form)]))])))

        (contains? #{:between :not-between} op)
        (let [[_ x lo hi] form]
          (when-not (= 4 (count form)) (throw (util/dsl-error (str "[" op " x lo hi]") form)))
          (cat-frags "" [(expr x ctx) (frag (if (= :between op) " BETWEEN " " NOT BETWEEN "))
                         (expr lo ctx) (frag " AND ") (expr hi ctx)]))

        (contains? #{:exists :not-exists} op)
        (let [[_ q] form]
          (when-not (and (= 2 (count form)) (subquery? q))
            (throw (util/dsl-error (str "[" op " {subquery}]") form)))
          (prefix (if (= :exists op) "EXISTS " "NOT EXISTS ") (paren (statement-sql q ctx))))

        (contains? infix-ops op)
        (let [args (rest form)]
          (when (and (contains? #{:-> :->>} op) (:sqlite-version ctx)
                     (not (util/version>= (:sqlite-version ctx) "3.38.0")))
            (throw (util/dsl-error (str op " needs SQLite >= 3.38.0") form {:requires "3.38.0"})))
          (cond
            (empty? args) (throw (util/dsl-error (str "[" op " ..] needs at least one argument") form))
            (and (= 1 (count args)) (= :- op)) (prefix "-" (paren (expr (first args) ctx)))
            (= 1 (count args)) (expr (first args) ctx)
            :else (paren (cat-frags (infix-ops op) (map #(expr % ctx) args)))))

        (= :cast op)
        (let [[_ x t] form]
          (when-not (and (= 3 (count form)) (contains? type-names t))
            (throw (util/dsl-error (str "[:cast x type] with type in " (pr-str (set (keys type-names)))) form)))
          (cat-frags "" [(frag "CAST(") (expr x ctx) (frag (str " AS " (type-names t) ")"))]))

        (= :as op)
        (let [[_ x alias] form]
          (when-not (and (= 3 (count form)) (ident-pos? alias))
            (throw (util/dsl-error "[:as expr alias]" form)))
          (cat-frags "" [(expr x ctx) (frag (str " AS " (util/sql-ident alias)))]))

        (= :distinct op)
        (do (when-not (= 2 (count form)) (throw (util/dsl-error "[:distinct x]" form)))
            (prefix "DISTINCT " (expr (second form) ctx)))

        (= :case op) (case-expr form ctx)

        (= :collate op)
        (let [[_ x c] form]
          (when-not (and (= 3 (count form)) (ident-pos? c)) (throw (util/dsl-error "[:collate expr :nocase]" form)))
          (cat-frags "" [(expr x ctx) (frag (str " COLLATE " (str/upper-case (util/kebab->snake c))))]))

        (= :over op)
        (let [[_ call spec] form]
          (when-not (and (<= 2 (count form) 3) (vector? call))
            (throw (util/dsl-error "[:over [fn args..] {:partition-by [..] :order-by [..]}]" form)))
          (cat-frags " " [(expr call ctx) (frag "OVER") (window-spec (or spec {}) ctx form)]))

        (= :json-each op)
        (let [[_ coll] form]
          (when-not (= 2 (count form)) (throw (util/dsl-error "[:json-each coll-or-expr]" form)))
          (if (sequential? coll)
            (paren-call "json_each" (frag "?" [(json-array-param coll form)]))
            (paren-call "json_each" (expr coll ctx))))

        (= :values op)
        (throw (util/dsl-error ":values is a statement clause, not an expression" form))

        :else (fn-call form ctx)))

    :else (value e ctx e)))

;; ---------------------------------------------------------------------------
;; SELECT
;; ---------------------------------------------------------------------------

(defn- result-name [item]
  (cond
    (keyword? item) (let [n (name item)] (when-not (str/ends-with? n "*") (util/kebab->snake (last (str/split n #"\.")))))
    (string? item) item
    :else nil))

(defn- select-item
  "-> {:sql :params :name}"
  [item ctx]
  (cond
    (ident-pos? item) (assoc (frag (util/sql-qualified item)) :name (result-name item))
    (or (number? item) (nil? item) (boolean? item)) (assoc (value item ctx item) :name nil)
    (subquery? item) (throw (util/dsl-error "a subquery select item needs an alias: [{..} :alias]" item))
    (vector? item)
    (let [n (count item)]
      (cond
        (empty? item) (throw (util/dsl-error "empty select item" item))
        (= :as (first item)) (assoc (expr item ctx) :name (util/ident-name (nth item 2)))
        (and (= 1 n) (not (keyword? (first item))))
        (assoc (expr (first item) ctx) :name nil)
        (and (= 2 n) (ident-pos? (second item)) (not= :* (second item))
             (not (contains? #{:raw :param :distinct :not :exists :not-exists :case :cast :over :json-each} (first item))))
        (let [[e alias] item]
          (assoc (cat-frags "" [(expr e ctx) (frag (str " AS " (util/sql-ident alias)))])
                 :name (util/ident-name alias)))
        :else (assoc (expr item ctx) :name nil)))
    :else (throw (util/dsl-error (str "invalid select item " (pr-str item)) item))))

(defn- select-list [items ctx form]
  (let [items (if (sequential? items) items [items])]
    (when (empty? items) (throw (util/dsl-error ":select needs at least one item" form)))
    (let [rendered (mapv #(select-item % ctx) items)
          names (remove nil? (map :name rendered))
          dups (->> names frequencies (filter #(> (val %) 1)) (map key))]
      (when (seq dups)
        (throw (util/dsl-error (str "duplicate result column name(s) " (pr-str (vec dups))
                                    ": the driver collapses duplicate keys into one; add aliases")
                               form {:duplicates (vec dups)})))
      (cat-frags ", " rendered))))

(defn- table-ref
  "Table position: :t | \"T\" | [:t :alias] | [{subquery} :alias] | {subquery} |
   [[:json-each x] :alias]."
  [t ctx form]
  (cond
    (ident-pos? t) (frag (util/sql-qualified t))
    (subquery? t) (paren (statement-sql t ctx))
    (and (vector? t) (= 2 (count t)) (ident-pos? (second t))
         (or (ident-pos? (first t)) (subquery? (first t)) (vector? (first t))))
    (let [[x alias] t
          base (cond (ident-pos? x) (frag (util/sql-qualified x))
                     (subquery? x) (paren (statement-sql x ctx))
                     :else (expr x ctx))]
      (cat-frags "" [base (frag (str " AS " (util/sql-ident alias)))]))
    (and (vector? t) (= 1 (count t)) (ident-pos? (first t))) (frag (util/sql-qualified (first t)))
    :else (throw (util/dsl-error (str "invalid table reference " (pr-str t)) form))))

(defn- alias-pair? [t]
  (and (vector? t) (= 2 (count t)) (ident-pos? (second t))
       (or (ident-pos? (first t)) (map? (first t)))))

(defn- from-clause
  ":from :t | [:t :alias] | [{sub} :alias] | {sub} | [item item ...]
   (a two-element [table alias] vector whose first element
   is a table name or subquery is ONE aliased table; anything else is a
   list of items, e.g. [[:users :u] [:orgs :o]] or [[[:json-each x] :j]])."
  [from ctx form]
  (let [items (cond
                (nil? from) nil
                (or (ident-pos? from) (map? from)) [from]
                (alias-pair? from) [from]
                (vector? from) from
                :else (throw (util/dsl-error (str "invalid :from " (pr-str from)) form)))]
    (when (seq items)
      (prefix "FROM " (cat-frags ", " (map #(table-ref % ctx form) items))))))

(def ^:private join-kinds
  {:join "INNER JOIN" :inner-join "INNER JOIN" :left-join "LEFT JOIN"
   :right-join "RIGHT JOIN" :full-join "FULL JOIN" :cross-join "CROSS JOIN"})

(defn- join-condition [condition ctx form]
  (cond
    (nil? condition) nil
    (and (vector? condition) (= :using (first condition)))
    (frag (str "USING (" (str/join ", " (map #(ident % form) (rest condition))) ")"))
    :else (prefix "ON " (expr condition ctx))))

(defn- one-join [kind table condition ctx form]
  (when (and (not= :cross-join kind) (nil? condition))
    (throw (util/dsl-error (str kind " needs a condition (expression or [:using ..]); use :cross-join for none") form)))
  (when (and (contains? #{:right-join :full-join} kind) (:sqlite-version ctx)
             (not (util/version>= (:sqlite-version ctx) "3.39.0")))
    (throw (util/dsl-error (str kind " needs SQLite >= 3.39.0") form {:requires "3.39.0"})))
  (cat-frags " " (remove nil? [(frag (join-kinds kind)) (table-ref table ctx form) (join-condition condition ctx form)])))

(defn- join-clauses [q ctx]
  (let [ordered (for [j (:joins q)]
                  (do (when-not (and (vector? j) (contains? join-kinds (first j)) (<= 2 (count j) 3))
                        (throw (util/dsl-error ":joins items are [kind table condition?]" j)))
                      (one-join (first j) (second j) (nth j 2 nil) ctx j)))
        keyed (for [kind [:join :inner-join :left-join :right-join :full-join :cross-join]
                    :let [spec (get q kind)]
                    :when spec
                    :let [_ (when-not (vector? spec) (throw (util/dsl-error (str kind " must be a vector") spec)))
                          pairs (if (= :cross-join kind)
                                  (map vector spec (repeat nil))
                                  (do (when (odd? (count spec))
                                        (throw (util/dsl-error (str kind " takes [table condition ...] pairs") spec)))
                                      (partition 2 spec)))]
                    [table condition] pairs]
                (one-join kind table condition ctx spec))]
    (seq (concat ordered keyed))))

(defn- cte-clause [with ctx form]
  (let [entries (cond
                  (map? with) (map (fn [[k v]] [k v]) with)
                  (vector? with) with
                  :else (throw (util/dsl-error ":with must be a vector of [name query opts?] or a map {name query}" form)))
        rendered (map (fn [entry]
                        (when-not (and (vector? entry) (<= 2 (count entry) 3))
                          (throw (util/dsl-error ":with entries are [name query opts?]" entry)))
                        (let [[head q opts] entry
                              [nm cols] (if (vector? head) [(first head) (second head)] [head nil])]
                          (when-not (map? q) (throw (util/dsl-error "a CTE body must be a query map" entry)))
                          {:recursive (boolean (:recursive opts))
                           :frag (cat-frags ""
                                            [(frag (str (util/sql-ident nm)
                                                        (when (seq cols) (str "(" (str/join ", " (map #(ident % entry) cols)) ")"))
                                                        " AS "
                                                        (case (:materialized opts)
                                                          true "MATERIALIZED " false "NOT MATERIALIZED " nil "")))
                                             (paren (statement-sql q ctx))])}))
                      entries)]
    (when (seq rendered)
      (prefix (if (some :recursive rendered) "WITH RECURSIVE " "WITH ")
              (cat-frags ", " (map :frag rendered))))))

(defn- limit-offset [q ctx form]
  (let [{:keys [limit offset]} q]
    (when (and (some? offset) (nil? limit))
      (throw (util/dsl-error ":offset requires :limit (SQLite has no OFFSET without LIMIT; use :limit -1 for no limit)" form)))
    (remove nil?
            [(when (some? limit) (prefix "LIMIT " (expr limit ctx)))
             (when (some? offset) (prefix "OFFSET " (expr offset ctx)))])))

(def ^:private select-keys*
  #{:with :select :distinct? :from :joins :join :inner-join :left-join :right-join :full-join :cross-join
    :where :group-by :having :order-by :limit :offset})

(def ^:private compound-keys #{:union :union-all :intersect :except})

(defn- check-keys [q allowed form-name]
  (let [unknown (remove allowed (keys q))]
    (when (seq unknown)
      (throw (util/dsl-error (str "unknown " form-name " clause(s) " (pr-str (vec unknown))) q {:unknown (vec unknown)})))))

(defn- order-by-clause
  ":order-by is a vector of items (or one item); see order-item."
  [items ctx]
  (when-let [items (seq (if (vector? items) items (when (some? items) [items])))]
    (prefix "ORDER BY " (cat-frags ", " (map #(order-item % ctx) items)))))

(defn- simple-select-sql [q ctx]
  (check-keys q select-keys* "select")
  (cat-frags " "
             (remove nil?
                     (concat
                      [(when (:with q) (cte-clause (:with q) ctx q))
                       (prefix (if (:distinct? q) "SELECT DISTINCT " "SELECT ") (select-list (:select q) ctx q))
                       (from-clause (:from q) ctx q)]
                      (join-clauses q ctx)
                      [(when (some? (:where q)) (prefix "WHERE " (expr (:where q) ctx)))
                       (when-let [g (:group-by q)]
                         (prefix "GROUP BY " (cat-frags ", " (map #(expr % ctx) (if (sequential? g) g [g])))))
                       (when (some? (:having q)) (prefix "HAVING " (expr (:having q) ctx)))
                       (order-by-clause (:order-by q) ctx)]
                      (limit-offset q ctx q)))))

(defn- compound-sql [q ctx]
  (check-keys q (into compound-keys #{:with :order-by :limit :offset}) "compound select")
  (let [kind (some compound-keys (keys q))
        members (get q kind)]
    (when-not (and (vector? members) (>= (count members) 2) (every? map? members))
      (throw (util/dsl-error (str kind " needs a vector of at least two query maps") q)))
    (doseq [m members]
      (when (some #(contains? m %) [:order-by :limit :offset])
        (throw (util/dsl-error "compound members cannot carry :order-by/:limit/:offset; put them on the compound" m))))
    (cat-frags " "
               (remove nil?
                       (concat
                        [(when (:with q) (cte-clause (:with q) ctx q))
                         (cat-frags (case kind :union " UNION " :union-all " UNION ALL "
                                          :intersect " INTERSECT " :except " EXCEPT ")
                                    (map #(select-sql % ctx) members))
                         (order-by-clause (:order-by q) ctx)]
                        (limit-offset q ctx q))))))

(defn select-sql
  "Renders a select or compound-select map -> {:sql :params}."
  [q ctx]
  (if (some compound-keys (keys q))
    (compound-sql q ctx)
    (do (when-not (contains? q :select)
          (throw (util/dsl-error "not a select: missing :select (or :union/:union-all/:intersect/:except)" q)))
        (simple-select-sql q ctx))))

;; ---------------------------------------------------------------------------
;; INSERT / UPSERT
;; ---------------------------------------------------------------------------

(def ^:private conflict-algorithms #{:rollback :abort :fail :ignore :replace})

(defn- or-clause [q]
  (when-let [o (:or q)]
    (when-not (contains? conflict-algorithms o)
      (throw (util/dsl-error (str ":or must be one of " (pr-str conflict-algorithms)) q)))
    (str "OR " (str/upper-case (name o)) " ")))

(defn- returning-clause [r ctx form]
  (when (some? r)
    (let [items (cond (= :* r) [:*] (ident-pos? r) [r] (sequential? r) r
                      :else (throw (util/dsl-error ":returning takes :* or a vector of items" form)))]
      (prefix "RETURNING " (select-list items ctx form)))))

(defn- set-clause [s ctx form]
  (let [pairs (cond (map? s) (seq s)
                    (and (vector? s) (every? #(and (vector? %) (= 2 (count %))) s)) s
                    :else (throw (util/dsl-error ":set must be a map {col expr} or a vector of [col expr] pairs" form)))]
    (when (empty? pairs) (throw (util/dsl-error ":set is empty" form)))
    (prefix "SET " (cat-frags ", " (map (fn [[k v]]
                                         (cat-frags "" [(frag (str (util/sql-ident k) " = ")) (expr v ctx)]))
                                       pairs)))))

(defn- do-update-set [s ctx form]
  (cond
    (and (vector? s) (seq s) (every? ident-pos? s))
    (frag (str "DO UPDATE SET "
               (str/join ", " (map #(str (util/sql-ident %) " = \"excluded\"." (util/sql-ident %)) s))))
    (map? s) (prefix "DO UPDATE " (set-clause s ctx form))
    :else (throw (util/dsl-error ":do-update-set is a vector of columns (col = excluded.col) or a map {col expr}" form))))

(defn- upsert-clause
  "One ON CONFLICT clause from {:target [cols] :target-where expr
   :do-update-set .. :where .. :do-nothing bool}."
  [{:keys [target target-where do-nothing] :as c} ctx form]
  (when-not (or do-nothing (contains? c :do-update-set))
    (throw (util/dsl-error "an ON CONFLICT clause needs :do-nothing true or :do-update-set" form)))
  (when (and do-nothing (contains? c :do-update-set))
    (throw (util/dsl-error ":do-nothing and :do-update-set are exclusive" form)))
  (when (and (some? target-where) (empty? target))
    (throw (util/dsl-error "a conflict target :where needs target columns" form)))
  (cat-frags " "
             (remove nil?
                     [(frag "ON CONFLICT")
                      (when (seq target) (frag (str "(" (str/join ", " (map #(ident % form) target)) ")")))
                      (when (some? target-where) (prefix "WHERE " (expr target-where ctx)))
                      (if do-nothing
                        (frag "DO NOTHING")
                        (do-update-set (:do-update-set c) ctx form))
                      (when (and (not do-nothing) (some? (:where c))) (prefix "WHERE " (expr (:where c) ctx)))])))

(defn- normalize-on-conflict
  "-> vector of clause maps (see upsert-clause) or nil when there is no upsert."
  [q]
  (let [oc (:on-conflict q)
        top {:do-update-set (:do-update-set q) :where (:where q) :do-nothing (:do-nothing q)}
        top (into {} (filter (comp some? val)) top)
        has-top? (or (contains? q :do-update-set) (:do-nothing q))
        pair? (fn [x] (and (vector? x) (= 2 (count x)) (vector? (first x)) (every? ident-pos? (first x)) (map? (second x))))
        pair->clause (fn [[target m]]
                       (check-keys m #{:where :do-update-set :do-nothing :update-where} "on-conflict")
                       (cond-> {:target target}
                         (contains? m :where) (assoc :target-where (:where m))
                         (contains? m :update-where) (assoc :where (:update-where m))
                         (contains? m :do-update-set) (assoc :do-update-set (:do-update-set m))
                         (:do-nothing m) (assoc :do-nothing true)))]
    (cond
      (and (nil? oc) (not has-top?)) (when (contains? q :where)
                                       (throw (util/dsl-error ":where on an insert is the DO UPDATE predicate and needs :do-update-set" q)))
      (nil? oc) [(assoc top :target [])]
      (and (vector? oc) (every? ident-pos? oc)) [(assoc top :target oc)]
      (pair? oc) (let [c (pair->clause oc)]
                   ;; single [target {:where ..}] pair: the DO clause comes from the
                   ;; pair map when present, else from the top-level keys
                   [(if (or (contains? c :do-update-set) (:do-nothing c)) c (merge c top))])
      (and (vector? oc) (seq oc) (every? pair? oc))
      (do (when has-top? (throw (util/dsl-error "with multiple :on-conflict clauses put :do-update-set/:do-nothing inside each clause" q)))
          (mapv pair->clause oc))
      :else (throw (util/dsl-error ":on-conflict is a vector of columns, a [cols {:where ..}] pair or a vector of [cols clause-map] pairs" q)))))

(def ^:private insert-keys*
  #{:with :insert-into :or :columns :values :select :default-values :on-conflict :do-update-set :do-nothing :where :returning})

(defn- insert-rows
  "-> [columns rows] with rows as vectors, validated."
  [q]
  (let [{:keys [columns values]} q]
    (when-not (and (sequential? values) (seq values))
      (throw (util/dsl-error ":values must be a non-empty vector of maps or vectors" q)))
    (cond
      (every? map? values)
      (let [cols (if (seq columns) (vec columns) (vec (keys (first values))))
            colset (set cols)]
        (when (seq columns)
          (doseq [c cols] (ident c q)))
        (doseq [row values]
          (when-not (= colset (set (keys row)))
            (throw (util/dsl-error "every :values map must have the same key set (SQLite VALUES has no DEFAULT keyword)"
                                   q {:expected (vec cols) :row row}))))
        [cols (mapv (fn [row] (mapv #(get row %) cols)) values)])
      (every? sequential? values)
      (do (when (empty? columns) (throw (util/dsl-error ":values as vectors need :columns" q)))
          (doseq [row values]
            (when-not (= (count row) (count columns))
              (throw (util/dsl-error "every :values row must have one value per :columns entry" q {:row row}))))
          [(vec columns) (mapv vec values)])
      :else (throw (util/dsl-error ":values must be all maps or all vectors" q)))))

(defn- insert-head [q ctx]
  (check-keys q insert-keys* "insert")
  (let [target (:insert-into q)]
    (cat-frags ""
               (remove nil?
                       [(when (:with q) (cat-frags "" [(cte-clause (:with q) ctx q) (frag " ")]))
                        (frag (str "INSERT " (or-clause q) "INTO "))
                        (table-ref target ctx q)]))))

(defn- insert-tail [q ctx]
  (let [clauses (normalize-on-conflict q)]
    (cat-frags " "
               (remove nil?
                       (concat
                        (map #(upsert-clause % ctx q) clauses)
                        [(returning-clause (:returning q) ctx q)])))))

(defn- values-rows-frag [cols rows ctx]
  (cat-frags ", "
             (map (fn [row] (paren (cat-frags ", " (map #(expr % ctx) row)))) rows)))

(defn insert-sql
  "Renders an :insert-into map -> {:sql :params}."
  [q ctx]
  (let [head (insert-head q ctx)
        tail (insert-tail q ctx)
        upsert? (or (:on-conflict q) (contains? q :do-update-set) (:do-nothing q))]
    (cond
      (:default-values q)
      (do (when (or upsert? (:values q) (:select q))
            (throw (util/dsl-error ":default-values cannot combine with :values/:select/upsert clauses" q)))
          (cat-frags " " (remove #(str/blank? (:sql %)) [head (frag "DEFAULT VALUES") tail])))

      (:select q)
      (let [cols (:columns q)
            sel (:select q)
            sel (if (and upsert? (map? sel) (not (contains? sel :where)) (not (some compound-keys (keys sel))))
                  (assoc sel :where [:raw "true"])
                  sel)]
        (when-not (map? sel) (throw (util/dsl-error ":select on an insert must be a query map" q)))
        (cat-frags " " (remove #(str/blank? (:sql %))
                               [head
                                (if (seq cols) (frag (str "(" (str/join ", " (map #(ident % q) cols)) ")")) (frag ""))
                                (select-sql sel ctx)
                                tail])))

      :else
      (let [[cols rows] (insert-rows q)]
        (cat-frags " " (remove #(str/blank? (:sql %))
                               [head
                                (frag (str "(" (str/join ", " (map #(ident % q) cols)) ")"))
                                (prefix "VALUES " (values-rows-frag cols rows ctx))
                                tail]))))))

;; ---------------------------------------------------------------------------
;; UPDATE / DELETE
;; ---------------------------------------------------------------------------

(def ^:private update-keys* #{:with :update :or :set :from :where :all? :order-by :limit :offset :returning})
(def ^:private delete-keys* #{:with :delete-from :where :all? :order-by :limit :offset :returning})

(defn- guard-where [q]
  (when (and (nil? (:where q)) (not (:all? q)))
    (throw (util/dsl-error "a :where-less :update/:delete-from touches every row; add :where or :all? true" q))))

(defn update-sql
  "Renders an :update map -> {:sql :params}."
  [q ctx]
  (check-keys q update-keys* "update")
  (guard-where q)
  (cat-frags " "
             (remove nil?
                     (concat
                      [(when (:with q) (cte-clause (:with q) ctx q))
                       (cat-frags "" [(frag (str "UPDATE " (or-clause q))) (table-ref (:update q) ctx q)])
                       (set-clause (:set q) ctx q)
                       (when-let [f (:from q)] (from-clause f ctx q))
                       (when (some? (:where q)) (prefix "WHERE " (expr (:where q) ctx)))
                       (returning-clause (:returning q) ctx q)
                       (order-by-clause (:order-by q) ctx)]
                      (limit-offset q ctx q)))))

(defn delete-sql
  "Renders a :delete-from map -> {:sql :params}."
  [q ctx]
  (check-keys q delete-keys* "delete")
  (guard-where q)
  (cat-frags " "
             (remove nil?
                     (concat
                      [(when (:with q) (cte-clause (:with q) ctx q))
                       (prefix "DELETE FROM " (table-ref (:delete-from q) ctx q))
                       (when (some? (:where q)) (prefix "WHERE " (expr (:where q) ctx)))
                       (returning-clause (:returning q) ctx q)
                       (order-by-clause (:order-by q) ctx)]
                      (limit-offset q ctx q)))))

;; ---------------------------------------------------------------------------
;; entry points
;; ---------------------------------------------------------------------------

(defn statement-kind
  "The statement kind of a DSL map: :select :insert :update :delete, or nil."
  [q]
  (when (map? q)
    (cond
      (contains? q :insert-into) :insert
      (contains? q :update) :update
      (contains? q :delete-from) :delete
      (or (contains? q :select) (some compound-keys (keys q))) :select
      :else nil)))

(defn- statement-sql [q ctx]
  (case (statement-kind q)
    :select (select-sql q ctx)
    :insert (insert-sql q ctx)
    :update (update-sql q ctx)
    :delete (delete-sql q ctx)
    (throw (util/dsl-error "not a statement: expected :select/:insert-into/:update/:delete-from (or :union ..)" q))))

(defn- ctx [opts] (merge default-opts opts))

(defn render
  "DSL map -> {:sql \"...\" :params [...]}. Pure. opts: :mode (:params
   default, :literal for DDL), :in-json-threshold, :sqlite-version (enables
   version gates), :max-params (a single statement above it is a
   dsl-error; see render-chunked for bulk inserts)."
  ([q] (render q {}))
  ([q opts]
   (when-not (map? q)
     (throw (util/dsl-error (str "render expects a statement map, got " (pr-str q)) q)))
   (let [c (ctx opts)
         r (statement-sql q c)
         limit (:max-params c)]
     (when (and (= :params (:mode c)) (> (count (:params r)) limit))
       (throw (util/dsl-error (str "statement needs " (count (:params r)) " parameters, above the limit of " limit
                                   " (bulk inserts: use render-chunked / query/execute!)")
                              q {:params (count (:params r)) :limit limit})))
     (when (and (= :literal (:mode c)) (seq (:params r)))
       (throw (util/dsl-error "literal mode produced parameters (internal error)" q)))
     (if (= :literal (:mode c)) (dissoc r :params) r))))

(defn render-expr
  "Expression -> {:sql :params}; opts as for render. Public for DDL and
   tests."
  ([e] (render-expr e {}))
  ([e opts]
   (let [c (ctx opts)
         r (expr e c)]
     (if (= :literal (:mode c)) (dissoc r :params) r))))

(defn render-literal
  "Expression rendered in DDL literal mode -> SQL string (no parameters
   possible: values are inlined)."
  [e]
  (:sql (render-expr e {:mode :literal})))

(defn render-chunked
  "Bulk insert: an :insert-into map with many :values rows -> vector of
   {:sql :params} statements, each within :max-params (default 32766)
   parameters. Any other statement renders as a one-element vector. Rows are
   never split; a row with more parameters than the limit is a dsl-error.
   Execute the chunks inside one transaction (query/execute! does)."
  ([q] (render-chunked q {}))
  ([q opts]
   (let [c (ctx opts)
         limit (:max-params c)]
     (if (and (= :insert (statement-kind q)) (:values q) (not (:default-values q)))
       (let [[cols rows] (insert-rows q)
             one (statement-sql (assoc q :values [(first (:values q))]) c)
             row-params (count (:params (values-rows-frag cols [(first rows)] c)))
             extra (- (count (:params one)) row-params)
             per-chunk (if (pos? row-params) (quot (- limit extra) row-params) (count rows))]
         (when (< per-chunk 1)
           (throw (util/dsl-error (str "one row needs " (+ extra row-params) " parameters, above the limit of " limit) q)))
         (mapv (fn [chunk] (render (assoc q :values (vec chunk)) opts))
               (partition-all per-chunk (:values q))))
       [(render q opts)]))))
