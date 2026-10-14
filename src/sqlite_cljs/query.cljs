(ns sqlite-cljs.query
  "Execute layer: runs DSL maps, {:sql :params} maps, [sql & params] vectors
   or plain SQL strings against a db value from sqlite-cljs.core and returns
   promesa promises. The driver is synchronous, so every promise is already
   settled on return; the promise API is kept so that an
   asynchronous driver can be swapped in later without touching call sites.

   Inside a transact! body (which must be synchronous) deref the settled
   promises to get values: @(execute! tx q). A rejected promise throws on
   deref, which is what makes the transaction roll back.

   Rows are vectors of kebab-keyed maps; values are raw SQLite values
   (INTEGER -> number, REAL -> number, TEXT -> string, BLOB -> Buffer,
   NULL -> nil). Per-call opts: {:coerce {:col :boolean|:date|:number|:json|fn}
   :safe-integers? bool}.

   Statement cache: prepared statements are cached per connection in a
   128-entry LRU keyed on the SQL text; exec! and
   migrate! clear it."
  (:require [promesa.core :as p]
            [sqlite-cljs.core :as core]
            [sqlite-cljs.ddl :as ddl]
            [sqlite-cljs.dsl :as dsl]
            [sqlite-cljs.util :as util]))

(defn- settle [thunk]
  (try (p/resolved (thunk))
       (catch :default e (p/rejected e))))

(defn ->statement
  "Normalises a query designator to {:sql \"..\" :params [..]}:
   string -> no params; [sql & params]; {:sql .. :params ..} as is; a DSL map
   is rendered by sqlite-cljs.dsl/render (opts :in-json-threshold,
   :max-params, :sqlite-version are forwarded). Pure."
  ([q] (->statement q {}))
  ([q opts]
   (cond
     (string? q) {:sql q :params []}
     (and (vector? q) (string? (first q))) {:sql (first q) :params (vec (rest q))}
     (and (map? q) (contains? q :sql))
     (do (when-not (string? (:sql q))
           (throw (util/argument-error ":sql must be a string" {:value q})))
         {:sql (:sql q) :params (vec (:params q))})
     (map? q) (dsl/render q (select-keys opts [:in-json-threshold :max-params :sqlite-version :mode]))
     :else (throw (util/argument-error (str "not a query: expected a DSL map, {:sql :params}, [sql & params] or a string; got " (pr-str q))
                                       {:value q})))))

(defn query-all!
  "Resolves to a vector of row maps. `q` is any query designator (see
   ->statement). opts: :coerce, :safe-integers?."
  ([d q] (query-all! d q {}))
  ([d q opts]
   (settle (fn [] (let [{:keys [sql params]} (->statement q opts)]
                    (core/all* d sql params opts))))))

(defn query-one!
  "Resolves to the first row map or nil."
  ([d q] (query-one! d q {}))
  ([d q opts]
   (settle (fn [] (let [{:keys [sql params]} (->statement q opts)]
                    (core/get* d sql params opts))))))

(defn query-value!
  "Resolves to the first column of the first row (or nil): handy for
   {:select [[[:count :*] :n]] ..}."
  ([d q] (query-value! d q {}))
  ([d q opts]
   (settle (fn [] (let [{:keys [sql params]} (->statement q opts)]
                    (some-> (core/get* d sql params opts) first val))))))

(defn- execute-one [d {:keys [sql params]} opts]
  (if (core/reader?* d sql)
    (core/all* d sql params opts)
    (core/run* d sql params opts)))

(defn- in-tx [d mode f]
  (if (core/in-transaction? d)
    (f)
    (core/transaction* d mode (fn [_] (f)))))

(defn execute!
  "Runs a write (or any statement). Resolves to {:changes n
   :last-insert-rowid n} or, when the statement has RETURNING, to the
   returned rows. A DSL insert whose :values exceed :max-params (32766)
   parameters is chunked into several statements inside one IMMEDIATE
   transaction (unless one is already active); :changes are summed and
   RETURNING rows concatenated. opts: :coerce :safe-integers? :max-params."
  ([d q] (execute! d q {}))
  ([d q opts]
   (settle
    (fn []
      (if (and (map? q) (= :insert (dsl/statement-kind q)) (:values q))
        (let [chunks (dsl/render-chunked q (select-keys opts [:in-json-threshold :max-params :sqlite-version]))]
          (if (= 1 (count chunks))
            (execute-one d (first chunks) opts)
            (in-tx d :immediate
                   (fn []
                     (reduce (fn [acc st]
                               (let [r (execute-one d st opts)]
                                 (if (vector? r)
                                   (into (if (vector? acc) acc []) r)
                                   {:changes (+ (:changes acc 0) (:changes r))
                                    :last-insert-rowid (:last-insert-rowid r)})))
                             {:changes 0 :last-insert-rowid nil}
                             chunks)))))
        (execute-one d (->statement q opts) opts))))))

(defn execute-all!
  "Runs a vector of statements (any designators, DDL definition maps
   included) in one IMMEDIATE transaction unless one is already active.
   Resolves to a vector of the individual results. DDL maps clear the
   statement cache."
  ([d qs] (execute-all! d qs {}))
  ([d qs opts]
   (settle
    (fn []
      (when-not (sequential? qs) (throw (util/argument-error "execute-all! takes a vector of statements" {:value qs})))
      (in-tx d :immediate
             (fn []
               (mapv (fn [q]
                       (if (ddl/def-kind q)
                         (let [{:keys [statements]} (ddl/render q {:sqlite-version (:sqlite-version opts)})]
                           (doseq [s statements] (core/run* d s []))
                           (core/clear-cache! d)
                           {:statements statements})
                         (execute-one d (->statement q opts) opts)))
                     qs)))))))

(defn exec!
  "Runs SQL with NO parameters: a string (may contain several statements —
   migrations, seeds), a vector of strings, or a vector of DDL definition
   maps (rendered with sqlite-cljs.ddl). Clears the statement cache.
   Resolves to nil. Prefer execute!/execute-all! for anything with values."
  [d sql-or-vec]
  (settle
   (fn []
     (cond
       (string? sql-or-vec) (core/exec* d sql-or-vec)
       (sequential? sql-or-vec)
       (doseq [s sql-or-vec]
         (cond
           (string? s) (core/exec* d s)
           (ddl/def-kind s) (doseq [st (:statements (ddl/render s))] (core/exec* d st))
           :else (throw (util/argument-error "exec! vector items are SQL strings or DDL definition maps" {:value s}))))
       (ddl/def-kind sql-or-vec) (doseq [st (:statements (ddl/render sql-or-vec))] (core/exec* d st))
       :else (throw (util/argument-error "exec! takes a SQL string, a vector of strings, or DDL maps" {:value sql-or-vec})))
     nil)))

(defn transact!
  "Runs (f tx) inside a transaction: (transact! d f) uses BEGIN IMMEDIATE
   (the default mode); (transact! d :deferred f) /
   :exclusive select the mode. `tx` is the same db value. The body runs
   synchronously; deref the settled promises inside it
   (@(execute! tx q)) and return a plain value, which becomes the resolved
   value. Any throw (including a deref of a rejected promise) rolls back
   and rejects. Nested transact! calls reject with
   :sqlite-cljs/argument-error {:reason :nested-transaction}; a body that
   returns a promise is rejected (and rolled back) with {:reason :async-body}."
  ([d f] (transact! d :immediate f))
  ([d mode f]
   (settle (fn []
             (when-not (fn? f) (throw (util/argument-error "transact! needs a function" {:value f})))
             (core/transaction* d mode f)))))

(defn clear-cache!
  "Drops every cached prepared statement of the connection. Resolves to nil."
  [d]
  (settle #(core/clear-cache! d)))
