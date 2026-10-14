(ns sqlite-cljs.core-test
  "Driver binding: lifecycle, pragmas, error mapping, value binding."
  (:require [cljs.test :refer [deftest is testing]]
            [promesa.core :as p]
            [sqlite-cljs.core :as core]
            [sqlite-cljs.query :as q]
            [sqlite-cljs.test-util :refer [rejection error-type settled-error] :refer-macros [deftest-p]]
            [sqlite-cljs.util :as util]))

(defn- cfg-err [thunk]
  (try (thunk) nil (catch :default e (ex-data e))))

(deftest-p open-close-lifecycle
  (let [d (core/db)]
    (is (map? d))
    (is (= ":memory:" (:path d)))
    (is (:memory? d))
    (is (core/open? d))
    (is (not (core/in-transaction? d)))
    (p/let [_ (core/close! d)]
      (is (not (core/open? d)))
      (p/let [_ (core/close! d)]
        (is true "double close is fine"))
      (p/let [t (error-type (q/query-all! d "SELECT 1 AS v"))
              [e] (rejection (q/query-all! d "SELECT 1 AS v"))]
        (is (= :sqlite-cljs/argument-error t) "using a closed connection is an argument-error")
        (is (= :closed (:reason (ex-data e))))))))

(deftest-p db-constructor-forms
  (let [a (core/db ":memory:")
        b (core/db {:memory? true})
        c (core/db nil)]
    (is (every? :memory? [a b c]))
    (is (= {:memory? true} (:open-opts b)))
    (core/close! a) (core/close! b) (core/close! c)
    (p/resolved nil)))

(deftest bad-open-options-are-config-errors
  (is (= :sqlite-cljs/config-error (:type (cfg-err #(core/db {:memory? true :readonly? true})))) "memory cannot be readonly")
  (is (= :sqlite-cljs/config-error (:type (cfg-err #(core/db {:memory? true :timeout-ms -1})))))
  (is (= :sqlite-cljs/config-error (:type (cfg-err #(core/db {:memory? true :journal-mode :sideways})))))
  (is (= :sqlite-cljs/config-error (:type (cfg-err #(core/db {:memory? true :synchronous :maybe})))))
  (is (= :sqlite-cljs/config-error (:type (cfg-err #(core/db 42)))))
  (is (= :sqlite-cljs/config-error (:type (cfg-err #(core/db "target/no-such-dir-xyz/x.db")))) "missing directory")
  (is (= "SQLITE_CANTOPEN" (:code (cfg-err #(core/db {:path "target/no-such-file-xyz.db" :file-must-exist? true}))))
      ":file-must-exist? surfaces SQLite's own code"))

(deftest-p effective-pragmas-on-memory
  (let [d (core/db)]
    (p/let [info (core/driver-info d)
            fk (core/pragma d :foreign-keys)
            jm (core/pragma d :journal-mode)
            bt (core/pragma d :busy-timeout {:rows? true})
            ts (core/pragma d :trusted-schema)]
      (is (= "memory" (:journal-mode (:effective d))) ":memory: databases ignore WAL and answer memory (no config-error)")
      (is (= 1 fk) "foreign_keys set explicitly on")
      (is (:foreign-keys? (:effective d)))
      (is (= "memory" jm))
      (is (= [{:timeout 5000}] bt) "busy_timeout row shape {:timeout n}")
      (is (= 5000 (:busy-timeout-ms (:effective d))))
      (is (= 0 ts) "trusted_schema off by default")
      (is (= 2 (:synchronous (:effective d))) "FULL is the non-WAL default")
      (is (= "better-sqlite3" (:driver info)))
      (is (re-matches #"\d+\.\d+\.\d+" (:driver-version info)))
      (is (re-matches #"3\.\d+\.\d+" (:sqlite-version info)))
      (is (:memory? info))
      (core/close! d))))

(deftest-p open-options-are-applied
  (let [d (core/db {:memory? true :foreign-keys? false :timeout-ms 250 :synchronous :off :trusted-schema? true})]
    (p/let [fk (core/pragma d :foreign-keys)
            sy (core/pragma d :synchronous)
            ts (core/pragma d :trusted-schema)]
      (is (= 0 fk))
      (is (not (:foreign-keys? (:effective d))))
      (is (= 250 (:busy-timeout-ms (:effective d))))
      (is (= 0 sy))
      (is (= 0 (:synchronous (:effective d))))
      (is (= 1 ts))
      (core/close! d))))

(deftest-p pragma-get-set-and-validation
  (let [d (core/db)]
    (p/let [v (core/pragma! d :user-version 7)
            uv (core/user-version d)
            cs (core/pragma! d :cache-size -4000)
            t1 (error-type (core/pragma d "user_version; DROP TABLE x"))
            t2 (error-type (core/pragma! d :journal-mode "WAL; DROP"))]
      (is (= 7 v))
      (is (= 7 uv))
      (is (= -4000 cs))
      (is (= :sqlite-cljs/argument-error t1) "pragma names are validated")
      (is (= :sqlite-cljs/argument-error t2) "pragma values are bare words only")
      (core/close! d))))

(deftest-p versions
  (let [d (core/db)]
    (p/let [sv (core/sqlite-version d)
            lv (core/library-version d)]
      (is (util/version>= sv "3.37.0") "STRICT tables need 3.37; the bundled SQLite is far newer")
      (is (util/version>= lv "13.0.0"))
      (core/close! d))))

(deftest-p sqlite-error-mapping
  (let [d (core/db)]
    (p/let [_ (q/exec! d "CREATE TABLE t (id INTEGER PRIMARY KEY, email TEXT NOT NULL UNIQUE, n INTEGER CHECK (n > 0)) STRICT")]
      (p/let [[e1] (rejection (q/execute! d ["INSERT INTO t (email, n) VALUES (?, ?)" "a" 1]))
              [e2] (rejection (q/execute! d ["INSERT INTO t (email, n) VALUES (?, ?)" "a" 1]))
              [e3] (rejection (q/execute! d ["INSERT INTO t (email, n) VALUES (?, ?)" "b" 0]))
              [e4] (rejection (q/execute! d ["INSERT INTO t (email, n) VALUES (?, ?)" nil 1]))
              [e5] (rejection (q/execute! d ["INSERT INTO t (email, n) VALUES (?, ?)" "c" "abc"]))
              [e6] (rejection (q/query-all! d "SELEC 1"))
              [e7] (rejection (q/query-all! d "SELECT * FROM nope"))]
        (is (nil? e1) "first insert of 'a' succeeds")
        (is (= :sqlite-cljs/sqlite-error (util/error-type e2)))
        (is (= "SQLITE_CONSTRAINT_UNIQUE" (util/sqlite-code e2)))
        (is (util/constraint-error? e2))
        (is (= "INSERT INTO t (email, n) VALUES (?, ?)" (:sql (ex-data e2))) "the SQL travels with the error")
        (is (= "SQLITE_CONSTRAINT_CHECK" (util/sqlite-code e3)))
        (is (= "SQLITE_CONSTRAINT_NOTNULL" (util/sqlite-code e4)))
        (is (= "SQLITE_CONSTRAINT_DATATYPE" (util/sqlite-code e5)) "STRICT type violation")
        (is (= "SQLITE_ERROR" (util/sqlite-code e6)))
        (is (re-find #"no such table" (:message (ex-data e7))))
        (core/close! d)))))

(deftest-p driver-misuse-is-argument-error
  (let [d (core/db)]
    (p/let [_ (q/exec! d "CREATE TABLE t (a INTEGER) STRICT")
            [e1] (rejection (q/query-all! d ["SELECT ? AS v"]))
            [e2] (rejection (q/query-all! d ["SELECT ? AS v" 1 2]))
            [e3] (rejection (q/query-all! d "SELECT 1; SELECT 2"))
            [e4] (rejection (q/query-all! d "INSERT INTO t VALUES (1)"))]
      (is (= :parameter-count (:reason (ex-data e1))))
      (is (= :parameter-count (:reason (ex-data e2))))
      (is (= :multiple-statements (:reason (ex-data e3))) "prepare takes one statement; use exec! for scripts")
      (is (= :not-a-reader (:reason (ex-data e4))) "query-all! on a write without RETURNING")
      (core/close! d))))

(deftest param-encoding
  (testing "->param conversions (inventory §5.7)"
    (is (nil? (core/->param nil)))
    (is (= "bigint" (goog/typeOf (core/->param 5))) "integral numbers bind as INTEGER (BigInt internally)")
    (is (= "5" (str (core/->param 5))))
    (is (= "-3" (str (core/->param -3))))
    (is (= 1.5 (core/->param 1.5)) "non-integral numbers bind as REAL")
    (is (= "1" (str (core/->param true))))
    (is (= "0" (str (core/->param false))))
    (is (= "x" (core/->param "x")))
    (is (= "active" (core/->param :active)) "keywords bind as their name")
    (is (= "9007199254740993" (str (core/->param (js/BigInt "9007199254740993")))))
    (let [b (js/Uint8Array. #js [1 2])] (is (identical? b (core/->param b)) "blobs are opaque")))
  (testing "refused values are argument-errors before any SQL runs"
    (let [reason (fn [v] (:reason (try (core/->param v) nil (catch :default e (ex-data e)))))]
      (is (= :infinity (reason js/Infinity)))
      (is (= :infinity (reason (- js/Infinity))))
      (is (= :nan (reason js/NaN)))
      (is (= :unsafe-integer (reason 9007199254740993)) "beyond 2^53: pass a BigInt")
      (is (= :date (reason (js/Date.))))
      (is (= :nul (reason "a\u0000b")))
      (is (= :collection (reason [1 2])))
      (is (= :collection (reason {:a 1})))
      (is (= :unbindable-value (reason #js {:a 1})))))
  (testing "->params"
    (is (= 3 (.-length (core/->params [1 "a" nil]))))
    (is (= :too-many-parameters (:reason (try (core/->params (vec (range 32767))) nil (catch :default e (ex-data e))))))))

(deftest-p integers-bind-as-integer-storage-class
  (let [d (core/db)]
    (p/let [r (q/query-one! d ["SELECT typeof(?) AS i, typeof(?) AS r, typeof(?) AS t, typeof(?) AS b, typeof(?) AS n, typeof(?) AS bl"
                               5 5.5 "s" true nil (js/Uint8Array. 1)])
            _ (q/exec! d "CREATE TABLE a (x ANY, s TEXT) STRICT")
            _ (q/execute! d ["INSERT INTO a VALUES (?, ?)" 5 5])
            row (q/query-one! d "SELECT typeof(x) AS tx, x, typeof(s) AS ts, s FROM a")]
      (is (= {:i "integer" :r "real" :t "text" :b "integer" :n "null" :bl "blob"} r))
      (is (= {:tx "integer" :x 5 :ts "text" :s "5"} row)
          "an ANY column keeps INTEGER 5 (a raw JS number would have become REAL 5.0 / TEXT '5.0')")
      (core/close! d))))

(deftest-p booleans-and-dates-round-trip-as-integers
  (let [d (core/db)
        now (js/Date.)]
    (p/let [_ (q/exec! d {:create-table :ev :columns [[:id :integer {:primary-key? true}]
                                                      [:active :integer {:not-null? true :default true}]
                                                      [:at :integer {:not-null? true}]]})
            _ (q/execute! d {:insert-into :ev :values [{:active false :at (.getTime now)} {:active true :at (.getTime now)}]})
            raw (q/query-all! d {:select [:active :at] :from :ev :order-by [:id]})
            coerced (q/query-all! d {:select [:id :active :at] :from :ev :order-by [:id]} {:coerce {:active :boolean :at :date}})
            hits (q/query-all! d {:select [:id] :from :ev :where [:= :active true]})]
      (is (= [{:active 0 :at (.getTime now)} {:active 1 :at (.getTime now)}] raw) "raw reads are 0/1 and epoch ms")
      (is (= [false true] (mapv :active coerced)) ":coerce :boolean")
      (is (every? #(instance? js/Date (:at %)) coerced) ":coerce :date")
      (is (= (.getTime now) (.getTime (:at (first coerced)))))
      (is (= [{:id 2}] hits) "true binds as 1 in WHERE")
      (core/close! d))))

(deftest-p safe-integers-bigint-reads
  (let [big (js/BigInt "9007199254740993")]
    (p/let [d (core/db)
            lossy (q/query-one! d ["SELECT ? AS v" big])
            exact (q/query-one! d ["SELECT ? AS v" big] {:safe-integers? true})
            r (q/execute! d "CREATE TABLE t (id INTEGER PRIMARY KEY) STRICT")
            ins (q/execute! d ["INSERT INTO t VALUES (?)" big] {:safe-integers? true})
            back (q/query-one! d "SELECT id FROM t" {:safe-integers? true})
            [coerce-err] (rejection (q/query-one! d "SELECT id FROM t" {:safe-integers? true :coerce {:id :number}}))]
      (is (= 9007199254740992 (:v lossy)) "default reads round beyond 2^53 (documented)")
      (is (= "bigint" (goog/typeOf (:v exact))))
      (is (= "9007199254740993" (str (:v exact))))
      (is (= "9007199254740993" (str (:last-insert-rowid ins))) "lastInsertRowid follows the switch")
      (is (= "9007199254740993" (str (:id back))) "exact read back")
      (is (= :unsafe-integer (:reason (ex-data coerce-err))) ":coerce :number refuses lossy conversion")
      (is (= :unsafe-integer (:reason (try (core/bigint->number big) nil (catch :default e (ex-data e))))))
      (is (= 42 (core/bigint->number (js/BigInt 42))))
      (is (= 42 (core/bigint->number 42)))
      (core/close! d))))

(deftest-p safe-integers-per-connection
  (let [d (core/db {:memory? true :safe-integers? true})]
    (p/let [r (q/query-one! d "SELECT 1 AS v, 1.5 AS f")
            n (q/query-one! d "SELECT 1 AS v" {:safe-integers? false})]
      (is (:safe-integers? (:effective d)))
      (is (= "bigint" (goog/typeOf (:v r))))
      (is (= 1.5 (:f r)) "REAL stays a number")
      (is (= 1 (:v n)) "per-query override")
      (core/close! d))))

(deftest-p coerce-kinds
  (let [d (core/db)]
    (p/let [row (q/query-one! d ["SELECT ? AS j, ? AS k, ? AS n, ? AS f, NULL AS z" "{\"a\":[1,2]}" "kw" 7 7]
                              {:coerce {:j :json :k :keyword :n :bigint :f (fn [v] (* 2 v)) :z :boolean}})]
      (is (= {:a [1 2]} (:j row)))
      (is (= :kw (:k row)))
      (is (= "bigint" (goog/typeOf (:n row))))
      (is (= 14 (:f row)))
      (is (nil? (:z row)) "NULL is never coerced")
      (core/close! d))))

(deftest-p user-defined-function
  (let [d (core/db)]
    (p/let [_ (core/define-function! d "shout" (fn [s] (str s "!")) {:deterministic? true})
            r (q/query-one! d ["SELECT shout(?) AS v" "hi"])
            t (error-type (core/define-function! d "bad name" identity))]
      (is (= "hi!" (:v r)))
      (is (= :sqlite-cljs/argument-error t))
      (core/close! d))))

(deftest-p file-database-wal-readonly-busy
  (let [path "target/sqlite-cljs-test.db"
        d (core/db {:path path :timeout-ms 50})]
    (p/let [info (core/driver-info d)
            _ (q/exec! d "DROP TABLE IF EXISTS t; CREATE TABLE t (a INTEGER) STRICT; DELETE FROM t")
            _ (q/execute! d ["INSERT INTO t VALUES (?)" 1])
            ro (p/resolved (core/db {:path path :readonly? true :file-must-exist? true}))
            ro-rows (q/query-all! ro "SELECT a FROM t")
            [ro-write] (rejection (q/execute! ro ["INSERT INTO t VALUES (?)" 2]))
            ro-info (core/driver-info ro)]
      (is (= "wal" (:journal-mode (:effective d))) "file databases default to WAL")
      (is (= "wal" (:journal-mode (:effective ro))) "WAL is persistent in the file")
      (is (contains? #{1 2} (:synchronous (:effective d))) "the switching connection may still read FULL (inventory §8.16)")
      (is (= 50 (:busy-timeout-ms (:effective d))))
      (is (not (:memory? info)))
      (is (:readonly? ro-info))
      (is (= [{:a 1}] ro-rows) "the read-only connection sees the committed row")
      (is (= "SQLITE_READONLY" (util/sqlite-code ro-write)) "writes on a read-only connection")
      ;; busy: hold the write lock on d, try to write from a second connection with a 50 ms timeout
      (p/let [d2 (p/resolved (core/db {:path path :timeout-ms 50}))
              captured (atom nil)
              _ (q/transact! d (fn [tx]
                                 @(q/execute! tx ["INSERT INTO t VALUES (?)" 3])
                                 (reset! captured (settled-error (q/execute! d2 ["INSERT INTO t VALUES (?)" 4])))
                                 :done))
              n (q/query-value! d {:select [[[:count :*] :n]] :from :t})]
        (is (util/busy-error? @captured) "second writer gets SQLITE_BUSY after busy_timeout")
        (is (= "SQLITE_BUSY" (util/sqlite-code @captured)))
        (is (= 2 n))
        (core/close! d2)
        (core/close! ro)
        (core/close! d)))))

(deftest-p journal-mode-option-and-check
  (let [path "target/sqlite-cljs-test-journal.db"
        d (core/db {:path path :journal-mode :delete})]
    (is (= "delete" (:journal-mode (:effective d))))
    (p/let [_ (core/close! d)
            d2 (p/resolved (core/db {:path path :journal-mode :truncate :require-journal-mode? false}))]
      (is (= "truncate" (:journal-mode (:effective d2))))
      (core/close! d2))))
