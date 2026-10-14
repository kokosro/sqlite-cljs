(ns sqlite-cljs.util-test
  (:require [cljs.test :refer [deftest is testing]]
            [clojure.string :as str]
            [sqlite-cljs.util :as util]))

(deftest kebab-snake-roundtrip
  (is (= "user_name" (util/kebab->snake :user-name)))
  (is (= "user_name" (util/kebab->snake 'user-name)))
  (is (= "already_sql" (util/kebab->snake "already_sql")) "strings are exact names")
  (is (= "id" (util/kebab->snake :users/id)) "namespace dropped")
  (is (= :user-name (util/snake->kebab "user_name")))
  (is (= :created-at (util/snake->kebab :created_at)))
  (is (= :sqlite-cljs/x (util/snake->kebab :sqlite-cljs/x)) "namespaced keywords untouched"))

(deftest transform-keys-honours-passthrough
  (is (= {:a-b {:c-d 1} :blob {:x_y 2}}
         (util/transform-keys util/snake->kebab #{:blob} {:a_b {:c_d 1} :blob {:x_y 2}})))
  (is (= [{:a-b 1}] (util/transform-keys util/snake->kebab [{:a_b 1}]))))

(deftest ident-name-and-validation
  (is (= "user_name" (util/ident-name :user-name)))
  (is (= "Weird Name" (util/ident-name "Weird Name")) "strings verbatim, case kept")
  (is (thrown-with-msg? js/Error #"empty identifier" (util/ident-name "")))
  (is (thrown-with-msg? js/Error #"NUL" (util/ident-name "a\u0000b")))
  (is (thrown-with-msg? js/Error #"not an identifier" (util/ident-name 42)))
  (is (thrown-with-msg? js/Error #"not an identifier" (util/ident-name nil)))
  (is (= :sqlite-cljs/argument-error
         (util/error-type (try (util/ident-name 42) (catch :default e e))))))

(deftest sql-ident-quotes-strictly
  (testing "plain names"
    (is (= "\"users\"" (util/sql-ident :users)))
    (is (= "\"user_name\"" (util/sql-ident :user-name)))
    (is (= "\"MixedCase\"" (util/sql-ident "MixedCase")))
    (is (= "\"select\"" (util/sql-ident :select)) "keywords are safe once quoted")
    (is (= "\"with space\"" (util/sql-ident "with space")))
    (is (= "\"ünïcödé\"" (util/sql-ident "ünïcödé"))))
  (testing "embedded double quotes are doubled"
    (is (= "\"we\"\"ird\"" (util/sql-ident "we\"ird")))
    (is (= "\"\"\"\"" (util/sql-ident "\"")) "a lone quote becomes \"\"\"\"")
    (is (= "\"a\"\"\"\"b\"" (util/sql-ident "a\"\"b"))))
  (testing "injection attempts stay a single inert identifier"
    (let [evil "x\"; DROP TABLE users; --"
          q (util/sql-ident evil)]
      (is (= "\"x\"\"; DROP TABLE users; --\"" q))
      (is (str/starts-with? q "\""))
      (is (str/ends-with? q "\""))
      ;; every interior quote is doubled: stripping the outer quotes and
      ;; un-doubling must give the original back, and nothing can close early
      (is (= evil (str/replace (subs q 1 (dec (count q))) "\"\"" "\""))))
    (is (= "\"users\"\" OR 1=1 --\"" (util/sql-ident "users\" OR 1=1 --")))
    (is (= "\"'single' quotes; are; data\"" (util/sql-ident "'single' quotes; are; data"))
        "single quotes and semicolons need no escaping inside a quoted identifier")
    (is (= "\"back`tick]bracket\"" (util/sql-ident "back`tick]bracket")))
    (is (= "\"line\nbreak\"" (util/sql-ident "line\nbreak")))
    (is (= "\"users.id\"" (util/sql-ident "users.id")) "sql-ident never splits on dots")
    (is (= "\"id\"" (util/sql-ident :users/id)) "sql-ident drops a keyword namespace (use sql-qualified)")))

(deftest sql-qualified-splits-references
  (is (= "\"users\".\"id\"" (util/sql-qualified :users/id)))
  (is (= "\"users\".\"id\"" (util/sql-qualified :users.id)))
  (is (= "\"user_name\"" (util/sql-qualified :user-name)))
  (is (= "\"main\".\"users\".\"id\"" (util/sql-qualified :main.users/id)))
  (is (= "\"users\".*" (util/sql-qualified :users/*)))
  (is (= "\"users\".*" (util/sql-qualified :users.*)))
  (is (= "*" (util/sql-qualified :*)))
  (is (= "\"a.b\"" (util/sql-qualified "a.b")) "strings are one exact identifier")
  ;; keyword parts are kebab designators: "-" becomes "_" (so "--" -> "__"); the
  ;; embedded quote is still doubled and the reference stays two inert identifiers
  (is (= "\"t\".\"x\"\"; __\"" (util/sql-qualified (keyword "t" "x\"; --"))))
  (is (= "\"x\"\"; --\"" (util/sql-ident "x\"; --")) "strings keep the dashes")
  (is (= "\"t\".\"x\"\"; --\"" (str (util/sql-ident :t) "." (util/sql-ident "x\"; --")))))

(deftest row-conversion-handles-null-prototype-objects
  (let [plain #js {:user_id 1 :first_name "a" :n nil}
        nullproto (js/Object.create nil)]
    (unchecked-set nullproto "created_at" 5)
    (unchecked-set nullproto "big" (js/BigInt "9007199254740993"))
    (is (= {:user-id 1 :first-name "a" :n nil} (util/row->clj plain)))
    (is (= 5 (:created-at (util/row->clj nullproto))))
    (is (= "9007199254740993" (str (:big (util/row->clj nullproto)))) "BigInt passes through")
    (is (nil? (util/row->clj nil)))
    (is (nil? (util/row->clj js/undefined)))
    (is (= [{:a 1} {:a 2}] (util/rows->clj #js [#js {:a 1} #js {:a 2}])))
    (is (= [] (util/rows->clj nil)))))

(deftest error-constructors-and-detection
  (let [cause (doto (js/Error. "UNIQUE constraint failed: t.a") (unchecked-set "code" "SQLITE_CONSTRAINT_UNIQUE"))
        sq (util/sqlite-error cause {:sql "insert into t"})
        busy (util/sqlite-error (doto (js/Error. "database is locked") (unchecked-set "code" "SQLITE_BUSY")))
        nocode (util/sqlite-error (js/Error. "plain"))
        arg (util/argument-error "bad" {:value 1})
        dsl (util/dsl-error "unknown clause" {:frob 1} {:clause :frob})
        cfg (util/config-error "no driver")
        mig (util/migration-error "gap" {:from 1 :to 3})]
    (is (= :sqlite-cljs/sqlite-error (util/error-type sq)))
    (is (= "SQLITE_CONSTRAINT_UNIQUE" (util/sqlite-code sq)))
    (is (= "SQLite error SQLITE_CONSTRAINT_UNIQUE: UNIQUE constraint failed: t.a" (ex-message sq)))
    (is (= "insert into t" (:sql (util/error-data sq))))
    (is (identical? cause (:cause (util/error-data sq))))
    (is (util/constraint-error? sq))
    (is (not (util/busy-error? sq)))
    (is (util/busy-error? busy))
    (is (nil? (util/sqlite-code nocode)))
    (is (= "SQLite error: plain" (ex-message nocode)))
    (is (= :sqlite-cljs/argument-error (util/error-type arg)))
    (is (= 1 (:value (ex-data arg))))
    (is (= :sqlite-cljs/dsl-error (util/error-type dsl)))
    (is (= {:frob 1} (:form (ex-data dsl))))
    (is (str/starts-with? (ex-message dsl) "sqlite-cljs DSL: unknown clause"))
    (is (= :sqlite-cljs/config-error (util/error-type cfg)))
    (is (= :sqlite-cljs/migration-error (util/error-type mig)))
    (is (= 3 (:to (ex-data mig))))
    (is (nil? (util/error-data (ex-info "other" {:type :something/else}))))
    (is (nil? (util/error-data (js/Error. "plain"))))
    (is (false? (util/constraint-error? (js/Error. "plain"))))))
