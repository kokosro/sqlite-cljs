(ns sqlite-cljs.ddl
  "Pure DDL renderer: table/index/view/trigger/alter/drop definition maps ->
   {:statements [\"CREATE TABLE ...\" ...]}.
   DDL never carries parameters (SQLite forbids them in CHECK/DEFAULT/view/
   partial-index expressions), so expressions are rendered by
   sqlite-cljs.dsl in literal mode; user data never reaches DDL.

   Table definition (all keys but :create-table and :columns optional):

     {:create-table :users
      :if-not-exists? true            ; default true
      :strict? true                   ; default true
      :without-rowid? false
      :temp? false
      :columns [[:id :integer {:primary-key? true}]
                [:email :text {:not-null? true :unique? true :collate :nocase}]
                [:active :integer {:not-null? true :default true}]      ; booleans are 0/1
                [:org-id :integer {:references {:table :orgs :column :id
                                                :on-delete :cascade :on-update :restrict
                                                :deferrable :initially-deferred}}]
                [:created-at :integer {:not-null? true
                                       :default [:raw \"(unixepoch('subsec') * 1000)\"]}]
                [:label :text {:generated-as {:expr [:|| :name \" \" :email] :stored? true}}]
                [:legacy [:raw \"VARCHAR(255)\"] {}]]                   ; :strict? false only
      :primary-key [:a :b]            ; composite PK (only when no column is :primary-key?)
      :unique [[:org-id :email] [:name]]
      :check [[:>= :age 0]]
      :foreign-keys [{:columns [:org-id] :table :orgs :ref-columns [:id]
                      :on-delete :cascade :on-update :no-action}]
      :indexes [[:users-email-idx [:email] {:unique? true}]
                [nil [:org-id [:created-at :desc]] {:where [:= :active 1]}]]}

   Column shape is [name type opts-map]. Column opts: :primary-key? (true |
   :asc | :desc), :autoincrement? (only INTEGER PRIMARY KEY, avoided per
   sqlite.org unless requested), :not-null?, :unique?, :check expr,
   :default (literal | :current-time/:current-date/:current-timestamp |
   [:raw ..] | expression), :collate (:binary :nocase :rtrim | string),
   :references {:table :column :on-delete :on-update :deferrable},
   :generated-as {:expr :stored?}, :on-conflict (conflict clause for
   PK/NOT NULL/UNIQUE), :name (CONSTRAINT name). The bare spellings
   (:primary-key, :not-null, :unique, :autoincrement, :references [t c]
   with :on-delete/:on-update beside it, :generated + :stored) are accepted
   as aliases.

   Types: :integer :int :real :text :blob :any, plus :numeric and
   [:raw \"VARCHAR(255)\"] only with :strict? false. :boolean/:datetime and
   any other name are rejected (:sqlite-cljs/argument-error naming the
   column): booleans are INTEGER 0/1, dates INTEGER epoch milliseconds.
   STRICT rule violations are argument-errors too.

   Other definitions: {:create-index name :on/:table :columns [..] :unique?
   :where/:partial-where :if-not-exists?}, {:create-view name :columns :as
   query :if-not-exists? :temp?}, {:create-trigger name :on table :timing
   :before|:after|:instead-of :event :insert|:delete|:update|[:update :of
   [cols]] :when expr :body [\"raw sql\" {:raw \"..\"} dsl-map ..]},
   {:alter-table t :rename-to | :rename-column [old new] | :add-column col |
   :drop-column c | :alter-column [c :set-not-null|:drop-not-null]},
   {:drop-table/:drop-index/:drop-view/:drop-trigger name :if-exists?
   (default true)}.

   Render options: {:sqlite-version \"3.53.4\"} enables version gates
   (rename-column >= 3.25, drop-column >= 3.35, alter-column >= 3.53);
   without it nothing is gated."
  (:require [clojure.string :as str]
            [sqlite-cljs.dsl :as dsl]
            [sqlite-cljs.util :as util]))

;; ---------------------------------------------------------------------------
;; vocabulary
;; ---------------------------------------------------------------------------

(def strict-types
  "Types allowed in STRICT tables."
  #{:integer :int :real :text :blob :any})

(def all-types
  "Types allowed in non-strict tables (the STRICT set plus :numeric)."
  (conj strict-types :numeric))

(def fk-actions
  {:no-action "NO ACTION" :restrict "RESTRICT" :set-null "SET NULL"
   :set-default "SET DEFAULT" :cascade "CASCADE"})

(def conflict-clauses
  {:rollback "ROLLBACK" :abort "ABORT" :fail "FAIL" :ignore "IGNORE" :replace "REPLACE"})

(def default-keywords
  {:current-time "CURRENT_TIME" :current-date "CURRENT_DATE"
   :current-timestamp "CURRENT_TIMESTAMP" :null "NULL" :true "TRUE" :false "FALSE"})

(def ^:private column-opt-aliases
  {:primary-key :primary-key? :not-null :not-null? :unique :unique?
   :autoincrement :autoincrement? :nullable :nullable?})

(def ^:private column-opt-keys
  #{:primary-key? :autoincrement? :not-null? :unique? :check :default :collate
    :references :generated-as :on-conflict :name :on-delete :on-update :deferrable
    :generated :stored :stored?})

(defn- arg-error [msg data form]
  (util/argument-error msg (merge {:form form} data)))

(defn- ident-like? [x] (or (keyword? x) (string? x)))

;; ---------------------------------------------------------------------------
;; columns
;; ---------------------------------------------------------------------------

(defn- normalize-column-opts [opts col]
  (when-not (or (nil? opts) (map? opts))
    (throw (arg-error "column opts must be a map: [name type {opts}]" {:column (first col)} col)))
  (let [o (reduce-kv (fn [m k v] (assoc m (get column-opt-aliases k k) v)) {} (or opts {}))
        unknown (remove column-opt-keys (keys o))]
    (when (seq unknown)
      (throw (arg-error (str "unknown column option(s) " (pr-str (vec unknown)))
                        {:column (first col) :unknown (vec unknown)} col)))
    (cond-> o
      (vector? (:references o))
      (assoc :references (cond-> {:table (first (:references o)) :column (second (:references o))}
                           (:on-delete o) (assoc :on-delete (:on-delete o))
                           (:on-update o) (assoc :on-update (:on-update o))
                           (contains? o :deferrable) (assoc :deferrable (:deferrable o))))
      (and (map? (:references o)) (:on-delete o) (not (:on-delete (:references o))))
      (assoc-in [:references :on-delete] (:on-delete o))
      (and (map? (:references o)) (:on-update o) (not (:on-update (:references o))))
      (assoc-in [:references :on-update] (:on-update o))
      (contains? o :generated)
      (assoc :generated-as {:expr (:generated o) :stored? (boolean (or (:stored o) (:stored? o)))})
      true (dissoc :on-delete :on-update :generated :stored :stored?))))

(defn type-sql
  "Column type -> SQL type name, validated against the table's strictness.
   Throws :sqlite-cljs/argument-error {:column ..} for :boolean, :datetime,
   :varchar and any other unknown type, for :numeric / [:raw ..] types in a
   STRICT table and for a missing type in a STRICT table."
  [t strict? col]
  (let [cname (first col)]
    (cond
      (nil? t)
      (if strict?
        (throw (arg-error (str "column " (pr-str cname) " needs a type in a STRICT table")
                          {:column cname :reason :missing-type} col))
        nil)
      (keyword? t)
      (cond
        (contains? strict-types t) (dsl/type-names t)
        (and (= :numeric t) (not strict?)) "NUMERIC"
        (= :numeric t)
        (throw (arg-error (str "column " (pr-str cname) ": NUMERIC is not allowed in a STRICT table (use :strict? false)")
                          {:column cname :column-type t :reason :strict-type} col))
        (contains? #{:boolean :bool} t)
        (throw (arg-error (str "column " (pr-str cname) ": there is no BOOLEAN type; use :integer (0/1)")
                          {:column cname :column-type t :reason :unknown-type} col))
        (contains? #{:datetime :date :timestamp :time} t)
        (throw (arg-error (str "column " (pr-str cname) ": there is no " (name t) " type; use :integer epoch milliseconds (or :text ISO-8601)")
                          {:column cname :column-type t :reason :unknown-type} col))
        :else
        (throw (arg-error (str "column " (pr-str cname) ": unknown type " (pr-str t) "; use one of " (pr-str all-types)
                               " (legacy names via [:raw \"VARCHAR(255)\"] with :strict? false)")
                          {:column cname :column-type t :reason :unknown-type} col)))
      (and (vector? t) (= :raw (first t)) (string? (second t)) (= 2 (count t)))
      (if strict?
        (throw (arg-error (str "column " (pr-str cname) ": raw type " (pr-str (second t)) " is only allowed with :strict? false")
                          {:column cname :column-type t :reason :strict-type} col))
        (second t))
      :else
      (throw (arg-error (str "column " (pr-str cname) ": invalid type " (pr-str t)) {:column cname :column-type t :reason :unknown-type} col)))))

(defn- default-sql [d col]
  (cond
    (nil? d) "NULL"
    (keyword? d)
    (or (default-keywords d)
        (throw (arg-error (str "column " (pr-str (first col)) ": :default keyword must be one of " (pr-str (set (keys default-keywords))))
                          {:column (first col) :value d} col)))
    (and (vector? d) (= :raw (first d))) (dsl/render-literal d)
    (vector? d) (str "(" (dsl/render-literal d) ")")
    (map? d) (throw (arg-error "a DEFAULT cannot be a subquery" {:column (first col)} col))
    :else (dsl/render-literal d)))

(defn- collate-sql [c col]
  (cond
    (keyword? c) (str/upper-case (util/kebab->snake c))
    (string? c) (do (when-not (re-matches #"[A-Za-z_][A-Za-z0-9_]*" c)
                      (throw (arg-error (str "invalid collation name " (pr-str c)) {:column (first col)} col)))
                    c)
    :else (throw (arg-error "collation must be a keyword or string" {:column (first col)} col))))

(defn- conflict-sql [c form]
  (when (some? c)
    (str " ON CONFLICT " (or (conflict-clauses c)
                             (throw (arg-error (str "unknown conflict clause " (pr-str c)) {:value c} form))))))

(defn- fk-action-sql [kind a form]
  (when (some? a)
    (str " " kind " " (or (fk-actions a)
                          (throw (arg-error (str "unknown foreign key action " (pr-str a)) {:value a} form))))))

(defn- deferrable-sql [d form]
  (case d
    nil ""
    false " NOT DEFERRABLE"
    true " DEFERRABLE"
    :initially-deferred " DEFERRABLE INITIALLY DEFERRED"
    :initially-immediate " DEFERRABLE INITIALLY IMMEDIATE"
    (throw (arg-error (str "unknown :deferrable " (pr-str d)) {:value d} form))))

(defn- references-sql [{:keys [table column columns on-delete on-update deferrable] :as r} form]
  (when-not (map? r) (throw (arg-error ":references must be {:table t :column c ..}" {:value r} form)))
  (when-not (ident-like? table) (throw (arg-error ":references needs a :table" {:value r} form)))
  (let [cols (cond (seq columns) columns (some? column) [column] :else nil)]
    (str "REFERENCES " (util/sql-qualified table)
         (when (seq cols) (str "(" (str/join ", " (map util/sql-ident cols)) ")"))
         (fk-action-sql "ON DELETE" on-delete form)
         (fk-action-sql "ON UPDATE" on-update form)
         (deferrable-sql deferrable form))))

(defn- constraint-name [n] (when n (str "CONSTRAINT " (util/sql-ident n) " ")))

(defn column-sql
  "Renders one [name type opts] column definition -> SQL fragment. ctx:
   {:strict? bool :without-rowid? bool :add-column? bool}."
  ([col] (column-sql col {:strict? true}))
  ([col {:keys [strict? without-rowid? add-column?] :as ctx}]
   (when-not (and (vector? col) (<= 1 (count col) 3) (ident-like? (first col)))
     (throw (arg-error (str "a column is [name type opts-map], got " (pr-str col)) {:column (first col)} col)))
   (let [[cname t opts] col
         o (normalize-column-opts opts col)
         type-name (type-sql t strict? col)
         pk (:primary-key? o)
         gen (:generated-as o)]
     (when (and pk (not (contains? #{true :asc :desc} pk)))
       (throw (arg-error ":primary-key? must be true, :asc or :desc" {:column cname} col)))
     (when (and (:autoincrement? o) (or (not pk) (not= :integer t)))
       (throw (arg-error (str "column " (pr-str cname) ": AUTOINCREMENT needs :integer with :primary-key? true")
                         {:column cname :reason :autoincrement} col)))
     (when (and (:autoincrement? o) without-rowid?)
       (throw (arg-error "AUTOINCREMENT is not allowed in a WITHOUT ROWID table" {:column cname :reason :autoincrement} col)))
     (when (and (some? gen) (not (map? gen)))
       (throw (arg-error ":generated-as must be {:expr .. :stored? bool}" {:column cname} col)))
     (when (and gen (or pk (contains? o :default)))
       (throw (arg-error (str "column " (pr-str cname) ": a generated column cannot have a DEFAULT or be the PRIMARY KEY")
                         {:column cname :reason :generated} col)))
     (when add-column?
       (when (or pk (:unique? o))
         (throw (arg-error "ADD COLUMN cannot add PRIMARY KEY or UNIQUE columns" {:column cname :reason :add-column} col)))
       (when (and (:not-null? o) (or (not (contains? o :default)) (nil? (:default o))))
         (throw (arg-error "ADD COLUMN with NOT NULL needs a non-NULL :default" {:column cname :reason :add-column} col)))
       (when (and (contains? o :default)
                  (let [d (:default o)]
                    (or (contains? #{:current-time :current-date :current-timestamp} d)
                        (and (vector? d) (not= :raw (first d)))
                        (and (vector? d) (= :raw (first d)) (str/starts-with? (str/trim (second d)) "(")))))
         (throw (arg-error "ADD COLUMN DEFAULT cannot be CURRENT_TIME/DATE/TIMESTAMP or a parenthesised expression"
                           {:column cname :reason :add-column} col)))
       (when (and gen (:stored? gen))
         (throw (arg-error "ADD COLUMN cannot add a STORED generated column" {:column cname :reason :add-column} col)))
       (when (and (:references o) (contains? o :default) (some? (:default o)))
         (throw (arg-error "ADD COLUMN with REFERENCES needs DEFAULT NULL (foreign keys on)" {:column cname :reason :add-column} col))))
     (str (util/sql-ident cname)
          (when type-name (str " " type-name))
          (when pk
            (str " " (constraint-name (:name o)) "PRIMARY KEY"
                 (case pk :asc " ASC" :desc " DESC" "")
                 (conflict-sql (:on-conflict o) col)
                 (when (:autoincrement? o) " AUTOINCREMENT")))
          (when (:not-null? o) (str " NOT NULL" (when-not pk (conflict-sql (:on-conflict o) col))))
          (when (:unique? o) (str " UNIQUE" (when-not (or pk (:not-null? o)) (conflict-sql (:on-conflict o) col))))
          (when (contains? o :check) (str " CHECK (" (dsl/render-literal (:check o)) ")"))
          (when (contains? o :default) (str " DEFAULT " (default-sql (:default o) col)))
          (when (:collate o) (str " COLLATE " (collate-sql (:collate o) col)))
          (when (:references o) (str " " (references-sql (:references o) col)))
          (when gen
            (str " GENERATED ALWAYS AS (" (dsl/render-literal (:expr gen)) ")"
                 (if (:stored? gen) " STORED" " VIRTUAL")))))))

;; ---------------------------------------------------------------------------
;; table constraints
;; ---------------------------------------------------------------------------

(defn- col-list [cols form]
  (when-not (and (sequential? cols) (seq cols) (every? ident-like? cols))
    (throw (arg-error "expected a non-empty vector of column names" {:value cols} form)))
  (str "(" (str/join ", " (map util/sql-ident cols)) ")"))

(defn constraint-sql
  "Renders one table constraint: [:unique [cols] opts?], [:primary-key [cols]
   opts?], [:check expr opts?], [:foreign-key [cols] [table col ..] opts?]
   or the map form {:columns .. :table .. :ref-columns .. :on-delete ..
   :on-update .. :deferrable .. :name ..} for foreign keys."
  [c]
  (cond
    (map? c)
    (let [{:keys [columns table ref-columns name]} c]
      (str (constraint-name name) "FOREIGN KEY " (col-list columns c) " "
           (references-sql (-> c (assoc :columns ref-columns) (dissoc :ref-columns :name)) c)))
    (and (vector? c) (keyword? (first c)))
    (let [[kind arg & more] c
          opts (if (map? (first more)) (first more) {})
          opts (if (and (= :foreign-key kind) (vector? (first more))) (if (map? (second more)) (second more) {}) opts)]
      (case kind
        :unique (str (constraint-name (:name opts)) "UNIQUE " (col-list arg c) (conflict-sql (:on-conflict opts) c))
        :primary-key (str (constraint-name (:name opts)) "PRIMARY KEY " (col-list arg c) (conflict-sql (:on-conflict opts) c))
        :check (str (constraint-name (:name opts)) "CHECK (" (dsl/render-literal arg) ")")
        :foreign-key (let [ref (first more)]
                       (when-not (and (vector? ref) (ident-like? (first ref)))
                         (throw (arg-error "[:foreign-key [cols] [table col ..] opts?]" {:value c} c)))
                       (str (constraint-name (:name opts)) "FOREIGN KEY " (col-list arg c) " "
                            (references-sql (merge {:table (first ref) :columns (vec (rest ref))}
                                                   (select-keys opts [:on-delete :on-update :deferrable]))
                                            c)))
        (throw (arg-error (str "unknown table constraint " (pr-str kind)) {:value c} c))))
    :else (throw (arg-error (str "invalid table constraint " (pr-str c)) {:value c} c))))

;; ---------------------------------------------------------------------------
;; CREATE TABLE
;; ---------------------------------------------------------------------------

(def ^:private create-table-keys
  #{:create-table :if-not-exists? :strict? :without-rowid? :temp? :columns
    :primary-key :unique :check :foreign-keys :constraints :indexes})

(defn- check-keys [d allowed what]
  (let [unknown (remove allowed (keys d))]
    (when (seq unknown)
      (throw (arg-error (str "unknown " what " key(s) " (pr-str (vec unknown))) {:unknown (vec unknown)} d)))))

(defn- index-name [table cols]
  (str (util/ident-name table) "_"
       (str/join "_" (map (fn [c] (util/ident-name (if (vector? c) (first c) c))) cols))
       "_idx"))

(defn create-table-sql
  "Renders the CREATE TABLE statement of a table definition (no indexes)."
  [d]
  (check-keys d create-table-keys "create-table")
  (let [{:keys [columns strict? without-rowid? temp?] :or {strict? true}} d
        if-not-exists? (get d :if-not-exists? true)
        table (:create-table d)]
    (when-not (ident-like? table) (throw (arg-error ":create-table needs a table name" {:value table} d)))
    (when-not (and (vector? columns) (seq columns))
      (throw (arg-error ":columns must be a non-empty vector of [name type opts]" {:value columns} d)))
    (let [ctx {:strict? strict? :without-rowid? without-rowid?}
          names (map #(util/ident-name (first %)) columns)
          dup (->> names (map str/lower-case) frequencies (filter #(> (val %) 1)) (map key))
          col-pks (filter #(:primary-key? (normalize-column-opts (nth % 2 nil) %)) columns)
          generated (filter #(let [o (normalize-column-opts (nth % 2 nil) %)] (:generated-as o)) columns)]
      (when (seq dup) (throw (arg-error (str "duplicate column name(s) " (pr-str (vec dup))) {:columns (vec dup)} d)))
      (when (> (count col-pks) 1)
        (throw (arg-error "more than one column has :primary-key? true; use a composite :primary-key [cols]"
                          {:columns (mapv first col-pks) :reason :primary-key} d)))
      (when (and (seq col-pks) (:primary-key d))
        (throw (arg-error "both a column :primary-key? and a table :primary-key are given" {:reason :primary-key} d)))
      (when (= (count generated) (count columns))
        (throw (arg-error "a table needs at least one non-generated column" {:reason :generated} d)))
      (when (and without-rowid? (empty? col-pks) (not (:primary-key d))
                 (not (some #(and (vector? %) (= :primary-key (first %))) (:constraints d))))
        (throw (arg-error "WITHOUT ROWID tables need a PRIMARY KEY" {:reason :without-rowid} d)))
      (let [constraints (concat
                         (when-let [pk (:primary-key d)] [[:primary-key pk]])
                         (map (fn [u] [:unique u]) (:unique d))
                         (map (fn [c] [:check c]) (:check d))
                         (map (fn [fk]
                                (when-not (map? fk) (throw (arg-error ":foreign-keys entries are maps" {:value fk} d)))
                                fk)
                              (:foreign-keys d))
                         (:constraints d))
            body (concat (map #(column-sql % ctx) columns)
                         (map constraint-sql constraints))
            options (cond-> []
                      without-rowid? (conj "WITHOUT ROWID")
                      strict? (conj "STRICT"))]
        (str "CREATE " (when temp? "TEMP ") "TABLE " (when if-not-exists? "IF NOT EXISTS ")
             (util/sql-qualified table) " (" (str/join ", " body) ")"
             (when (seq options) (str " " (str/join ", " options))))))))

;; ---------------------------------------------------------------------------
;; CREATE INDEX / VIEW / TRIGGER
;; ---------------------------------------------------------------------------

(defn- indexed-column [c form]
  (cond
    (ident-like? c) (util/sql-qualified c)
    (and (vector? c) (ident-like? (first c)) (every? #{:asc :desc} (rest c)) (seq (rest c)))
    (str (util/sql-qualified (first c)) " " (str/upper-case (name (second c))))
    (and (vector? c) (vector? (first c)))
    (let [[e & flags] c
          collate (second (first (filter #(and (vector? %) (= :collate (first %))) flags)))
          flags (remove vector? flags)]
      (when-not (every? #{:asc :desc :collate :nocase :binary :rtrim} flags)
        (throw (arg-error "index expression flags are :asc/:desc/[:collate x]" {:value c} form)))
      (str (dsl/render-literal e)
           (when collate (str " COLLATE " (str/upper-case (util/kebab->snake collate))))
           (when-let [dir (some #{:asc :desc} flags)] (str " " (str/upper-case (name dir))))))
    (vector? c) (dsl/render-literal c)
    :else (throw (arg-error (str "invalid index column " (pr-str c)) {:value c} form))))

(def ^:private create-index-keys
  #{:create-index :on :table :columns :unique? :where :partial-where :if-not-exists?})

(defn create-index-sql
  "Renders {:create-index name :on table :columns [..] :unique? :where ..}.
   Index columns: :col | [:col :desc] | [[expr] :asc [:collate :nocase]] |
   [expr-vector]. nil name -> <table>_<cols>_idx."
  [d]
  (check-keys d create-index-keys "create-index")
  (let [table (or (:on d) (:table d))
        cols (:columns d)
        nm (or (:create-index d) (index-name table cols))
        where (or (:where d) (:partial-where d))]
    (when-not (ident-like? table) (throw (arg-error ":create-index needs :on table" {:value d} d)))
    (when-not (and (sequential? cols) (seq cols)) (throw (arg-error ":create-index needs :columns" {:value d} d)))
    (str "CREATE " (when (:unique? d) "UNIQUE ") "INDEX " (when (get d :if-not-exists? true) "IF NOT EXISTS ")
         (util/sql-qualified nm) " ON " (util/sql-qualified table)
         " (" (str/join ", " (map #(indexed-column % d) cols)) ")"
         (when (some? where) (str " WHERE " (dsl/render-literal where))))))

(defn- table-index-sql [table [nm cols opts]]
  (create-index-sql (cond-> {:create-index nm :on table :columns cols}
                      (:unique? opts) (assoc :unique? true)
                      (:unique opts) (assoc :unique? true)
                      (some? (:where opts)) (assoc :where (:where opts))
                      (contains? opts :if-not-exists?) (assoc :if-not-exists? (:if-not-exists? opts)))))

(def ^:private create-view-keys #{:create-view :columns :as :if-not-exists? :temp?})

(defn create-view-sql
  "Renders {:create-view name :columns [..] :as {select ..}}; the query is
   rendered in literal mode (views cannot take parameters)."
  [d]
  (check-keys d create-view-keys "create-view")
  (let [{:keys [columns temp?]} d
        nm (:create-view d)
        q (:as d)]
    (when-not (ident-like? nm) (throw (arg-error ":create-view needs a name" {:value d} d)))
    (when-not (map? q) (throw (arg-error ":create-view needs :as {select ..}" {:value d} d)))
    (str "CREATE " (when temp? "TEMP ") "VIEW " (when (get d :if-not-exists? true) "IF NOT EXISTS ")
         (util/sql-qualified nm)
         (when (seq columns) (str " (" (str/join ", " (map util/sql-ident columns)) ")"))
         " AS " (:sql (dsl/render q {:mode :literal})))))

(def ^:private create-trigger-keys
  #{:create-trigger :on :timing :event :for-each-row? :when :body :if-not-exists? :temp?})

(defn- trigger-body-sql [stmt form]
  (cond
    (string? stmt) (str/trim stmt)
    (and (map? stmt) (contains? stmt :raw)) (str/trim (str (:raw stmt)))
    (and (vector? stmt) (= :raw (first stmt))) (str/trim (str (second stmt)))
    (map? stmt) (:sql (dsl/render stmt {:mode :literal}))
    :else (throw (arg-error "trigger :body statements are raw SQL strings, {:raw ..} or DSL maps" {:value stmt} form))))

(defn create-trigger-sql
  "Renders {:create-trigger name :on table :timing :after :event :insert
   :when expr :body [\"UPDATE ...\" ...]} (body statements are raw
   strings; DSL maps in literal mode are accepted too)."
  [d]
  (check-keys d create-trigger-keys "create-trigger")
  (let [{:keys [on timing event body temp?]} d
        nm (:create-trigger d)
        [ev of] (if (vector? event) [(first event) (nth event 2 nil)] [event nil])]
    (when-not (ident-like? nm) (throw (arg-error ":create-trigger needs a name" {:value d} d)))
    (when-not (ident-like? on) (throw (arg-error ":create-trigger needs :on table" {:value d} d)))
    (when-not (contains? #{nil :before :after :instead-of} timing)
      (throw (arg-error ":timing must be :before, :after or :instead-of" {:value timing} d)))
    (when-not (contains? #{:insert :delete :update} ev)
      (throw (arg-error ":event must be :insert, :delete, :update or [:update :of [cols]]" {:value event} d)))
    (when-not (and (sequential? body) (seq body))
      (throw (arg-error ":body must be a non-empty vector of statements" {:value body} d)))
    (str "CREATE " (when temp? "TEMP ") "TRIGGER " (when (get d :if-not-exists? true) "IF NOT EXISTS ")
         (util/sql-qualified nm)
         (case timing :before " BEFORE" :after " AFTER" :instead-of " INSTEAD OF" "")
         " " (str/upper-case (name ev))
         (when (seq of) (str " OF " (str/join ", " (map util/sql-ident of))))
         " ON " (util/sql-qualified on)
         (when (:for-each-row? d) " FOR EACH ROW")
         (when (some? (:when d)) (str " WHEN " (dsl/render-literal (:when d))))
         " BEGIN " (str/join " " (map #(let [s (trigger-body-sql % d)] (if (str/ends-with? s ";") s (str s ";"))) body))
         " END")))

;; ---------------------------------------------------------------------------
;; ALTER / DROP
;; ---------------------------------------------------------------------------

(defn- gate [feature need version form]
  (when (and version (not (util/version>= version need)))
    (throw (util/dsl-error (str feature " needs SQLite >= " need " (connection has " version ")")
                           form {:requires need :sqlite-version version}))))

(def ^:private alter-keys #{:alter-table :rename-to :rename-column :add-column :drop-column :alter-column :strict?})

(defn alter-table-sql
  "Renders one :alter-table map (exactly one action). opts
   {:sqlite-version ..} gates rename-column (3.25), drop-column (3.35) and
   alter-column (3.53). :strict? (default true) validates the added column
   type as for a STRICT table."
  ([d] (alter-table-sql d {}))
  ([d {:keys [sqlite-version]}]
   (check-keys d alter-keys "alter-table")
   (let [table (:alter-table d)
         actions (filter #(contains? d %) [:rename-to :rename-column :add-column :drop-column :alter-column])]
     (when-not (ident-like? table) (throw (arg-error ":alter-table needs a table name" {:value d} d)))
     (when-not (= 1 (count actions))
       (throw (arg-error "an :alter-table map takes exactly one action (:rename-to :rename-column :add-column :drop-column :alter-column)"
                         {:actions (vec actions)} d)))
     (let [head (str "ALTER TABLE " (util/sql-qualified table) " ")
           action (first actions)
           v (get d action)]
       (case action
         :rename-to (do (when-not (ident-like? v) (throw (arg-error ":rename-to needs a name" {:value v} d)))
                        (str head "RENAME TO " (util/sql-ident v)))
         :rename-column (do (gate "RENAME COLUMN" "3.25.0" sqlite-version d)
                            (when-not (and (vector? v) (= 2 (count v)) (every? ident-like? v))
                              (throw (arg-error ":rename-column takes [old new]" {:value v} d)))
                            (str head "RENAME COLUMN " (util/sql-ident (first v)) " TO " (util/sql-ident (second v))))
         :add-column (str head "ADD COLUMN " (column-sql v {:strict? (get d :strict? true) :add-column? true}))
         :drop-column (do (gate "DROP COLUMN" "3.35.0" sqlite-version d)
                          (when-not (ident-like? v) (throw (arg-error ":drop-column needs a column name" {:value v} d)))
                          (str head "DROP COLUMN " (util/sql-ident v)))
         :alter-column (do (gate "ALTER COLUMN SET/DROP NOT NULL" "3.53.0" sqlite-version d)
                           (when-not (and (vector? v) (= 2 (count v)) (ident-like? (first v))
                                          (contains? #{:set-not-null :drop-not-null} (second v)))
                             (throw (arg-error ":alter-column takes [col :set-not-null|:drop-not-null]" {:value v} d)))
                           (str head "ALTER COLUMN " (util/sql-ident (first v))
                                (if (= :set-not-null (second v)) " SET NOT NULL" " DROP NOT NULL"))))))))

(def ^:private drop-kinds {:drop-table "TABLE" :drop-index "INDEX" :drop-view "VIEW" :drop-trigger "TRIGGER"})

(defn drop-sql
  "Renders {:drop-table/:drop-index/:drop-view/:drop-trigger name :if-exists?
   (default true)}."
  [d]
  (let [kind (some (set (keys drop-kinds)) (keys d))]
    (check-keys d #{kind :if-exists?} (name kind))
    (when-not (ident-like? (get d kind)) (throw (arg-error (str kind " needs a name") {:value d} d)))
    (str "DROP " (drop-kinds kind) " " (when (get d :if-exists? true) "IF EXISTS ") (util/sql-qualified (get d kind)))))

;; ---------------------------------------------------------------------------
;; entry points
;; ---------------------------------------------------------------------------

(defn def-kind
  "The kind of a DDL definition map: :create-table :create-index :create-view
   :create-trigger :alter-table :drop-table :drop-index :drop-view
   :drop-trigger, or nil."
  [d]
  (when (map? d)
    (some #(when (contains? d %) %)
          [:create-table :create-index :create-view :create-trigger :alter-table
           :drop-table :drop-index :drop-view :drop-trigger])))

(defn def-name
  "The object name a definition creates/alters/drops (keyword or string)."
  [d]
  (let [k (def-kind d)]
    (if (= :create-index k)
      (or (:create-index d) (index-name (or (:on d) (:table d)) (:columns d)))
      (get d k))))

(defn render
  "DDL definition map -> {:statements [\"sql\" ...]} (one SQL string per
   statement, never any parameters). A :create-table with :indexes renders
   the table statement followed by one CREATE INDEX per entry. opts:
   {:sqlite-version ..} for version gates."
  ([d] (render d {}))
  ([d opts]
   (when-not (map? d) (throw (arg-error (str "expected a DDL definition map, got " (pr-str d)) {} d)))
   (let [kind (def-kind d)]
     {:statements
      (case kind
        :create-table (into [(create-table-sql d)]
                            (map #(table-index-sql (:create-table d) %) (:indexes d)))
        :create-index [(create-index-sql d)]
        :create-view [(create-view-sql d)]
        :create-trigger [(create-trigger-sql d)]
        :alter-table [(alter-table-sql d opts)]
        (:drop-table :drop-index :drop-view :drop-trigger) [(drop-sql d)]
        (throw (arg-error "not a DDL definition: expected :create-table/:create-index/:create-view/:create-trigger/:alter-table/:drop-*"
                          {} d)))})))

(defn render-table-def
  "Alias of `render` named for its main use: (render-table-def table-def)
   -> {:statements [..]}. Accepts every definition kind."
  ([d] (render d {}))
  ([d opts] (render d opts)))

(defn expected-columns
  "For migrate!: the [{:name \"snake_name\" :type \"INTEGER\"} ..] a
   :create-table definition implies (types as SQLite reports them in
   table_xinfo: upper-case for the vocabulary, verbatim for [:raw ..])."
  [d]
  (let [strict? (get d :strict? true)]
    (mapv (fn [col]
            {:name (util/ident-name (first col))
             :type (or (type-sql (second col) strict? col) "")})
          (:columns d))))
