(ns sqlite-cljs.migrate-test
  "migrate!: creation, idempotency, drift detection, alter steps, user_version."
  (:require [cljs.test :refer [deftest is testing]]
            [promesa.core :as p]
            [sqlite-cljs.core :as core]
            [sqlite-cljs.migrate :as migrate]
            [sqlite-cljs.query :as q]
            [sqlite-cljs.test-util :refer [rejection error-type] :refer-macros [deftest-p]]
            [sqlite-cljs.util :as util]))

(def defs
  [{:create-table :orgs :columns [[:id :integer {:primary-key? true}] [:name :text {:not-null? true :unique? true}]]}
   {:create-table :users
    :columns [[:id :integer {:primary-key? true}]
              [:email :text {:not-null? true}]
              [:org-id :integer {:references {:table :orgs :column :id}}]]
    :indexes [[:users-org-idx [:org-id]]]}
   {:create-index :users-email-idx :on :users :columns [:email] :unique? true}
   {:create-view :user-emails :columns [:email] :as {:select [:email] :from :users}}
   {:create-trigger :users-audit :on :users :timing :after :event :insert
    :body ["INSERT INTO orgs (name) SELECT 'auto' WHERE NOT EXISTS (SELECT 1 FROM orgs WHERE name = 'auto')"]}])

(deftest-p migrate-creates-then-is-idempotent
  (p/let [d (core/db)
          r1 (migrate/migrate! d defs)
          st (migrate/status! d)
          r2 (migrate/migrate! d defs)
          uv (core/user-version d)]
    (is (= [:orgs :users :users-org-idx :users-email-idx :user-emails :users-audit] (:created r1)))
    (is (= [] (:existing r1)))
    (is (= 5 (:user-version r1)))
    (is (= 0 (:previous-user-version r1)))
    (is (string? (:sqlite-version r1)))
    (is (= ["orgs" "users"] (:tables st)))
    (is (= ["user_emails"] (:views st)))
    (is (= ["users_email_idx" "users_org_idx"] (:indexes st)))
    (is (= ["users_audit"] (:triggers st)))
    (is (= 5 (:user-version st)))
    (is (= [] (:created r2)) "second run creates nothing")
    (is (= [:orgs :users :users-org-idx :users-email-idx :user-emails :users-audit] (:existing r2)))
    (is (= 5 uv) "user_version = (count defs)")
    (core/close! d)))

(deftest-p migrate-adopts-existing-tables-and-adds-missing-indexes
  (p/let [d (core/db)
          _ (q/exec! d "CREATE TABLE orgs (id INTEGER PRIMARY KEY, name TEXT NOT NULL UNIQUE, extra TEXT) STRICT")
          r (migrate/migrate! d (subvec defs 0 2))
          ok (q/execute! d {:insert-into :orgs :values [{:name "a"}]})]
    (is (= [:orgs] (:existing r)) "extra columns in the database are not drift")
    (is (= [:users :users-org-idx] (:created r)))
    (is (= 1 (:changes ok)))
    (core/close! d)))

(deftest-p migrate-detects-drift
  (p/let [d (core/db)
          _ (q/exec! d "CREATE TABLE users (id INTEGER PRIMARY KEY, email INTEGER, org_id INTEGER) STRICT")
          [e] (rejection (migrate/migrate! d defs))
          uv (core/user-version d)
          tables (migrate/status! d)]
    (is (= :sqlite-cljs/migration-error (util/error-type e)))
    (is (= :drift (:reason (ex-data e))))
    (is (= :users (:table (ex-data e))))
    (is (= [] (:missing (ex-data e))))
    (is (= [{:column "email" :expected "TEXT" :actual "INTEGER"}] (:type-mismatch (ex-data e))))
    (is (= 0 uv) "nothing committed: user_version untouched")
    (is (= ["users"] (:tables tables)) "…and orgs (earlier in the vector) was rolled back")
    (core/close! d)))

(deftest-p migrate-reports-a-missing-column-as-drift-or-statement-failure
  (p/let [d (core/db)
          _ (q/exec! d "CREATE TABLE users (id INTEGER PRIMARY KEY, email TEXT) STRICT")
          [e1] (rejection (migrate/migrate! d (subvec defs 0 2)))
          [e2] (rejection (migrate/migrate! d [(first defs) (-> (second defs) (dissoc :indexes) (update :columns conj [:extra :text {}]))]))
          st (migrate/status! d)]
    (is (= :statement-failed (:reason (ex-data e1))) "the users_org_idx index fails before verification: SQLite's error travels with the def")
    (is (= 1 (:index (ex-data e1))))
    (is (= "SQLITE_ERROR" (:code (ex-data e1))))
    (is (= :sqlite-cljs/sqlite-error (util/error-type (:cause (ex-data e1)))))
    (is (= :drift (:reason (ex-data e2))))
    (is (= ["org_id" "extra"] (:missing (ex-data e2))))
    (is (= ["users"] (:tables st)) "rolled back both times")
    (core/close! d)))

(def defs-v2
  "The evolved schema: orgs renamed, users gains nickname and email->mail,
   view and trigger dropped. Table defs describe the desired shape; the
   alter defs bring a v1 database there; renames precede the renamed def."
  [{:alter-table :orgs :rename-to :organisations}
   {:create-table :organisations :columns [[:id :integer {:primary-key? true}] [:name :text {:not-null? true :unique? true}]]}
   {:create-table :users
    :columns [[:id :integer {:primary-key? true}]
              [:mail :text {:not-null? true}]
              [:org-id :integer {:references {:table :organisations :column :id}}]
              [:nickname :text {:not-null? true :default ""}]]
    :indexes [[:users-org-idx [:org-id]]]}
   {:alter-table :users :add-column [:nickname :text {:default "" :not-null? true}]}
   {:alter-table :users :rename-column [:email :mail]}
   {:create-index :users-mail-idx :on :users :columns [:mail] :unique? true}
   {:drop-view :user-emails}
   {:drop-trigger :users-audit}])

(deftest-p migrate-alter-steps-evolve-a-v1-database-idempotently
  (p/let [d (core/db)
          _ (migrate/migrate! d defs)
          _ (q/execute! d {:insert-into :orgs :values [{:name "acme"}]})
          _ (q/execute! d {:insert-into :users :values [{:email "a@x" :org-id 1}]})
          r1 (migrate/migrate! d defs-v2)
          cols (core/pragma d :table-info {:arg :users :rows? true})
          rows (q/query-all! d {:select [:mail :nickname :org-id] :from :users})
          r2 (migrate/migrate! d defs-v2)
          st (migrate/status! d)]
    (is (= [:orgs :users :users] (:altered r1)) "rename table, add column, rename column")
    (is (= [:users-mail-idx] (:created r1)))
    (is (= [:organisations :users :users-org-idx] (:existing r1)))
    (is (= [:user-emails :users-audit] (:dropped r1)))
    (is (= ["id" "mail" "org_id" "nickname"] (mapv :name cols)))
    (is (= [{:mail "a@x" :nickname "" :org-id 1}] rows) "data survived")
    (is (= [] (:altered r2)) "re-run: every alter step already applied")
    (is (= [] (:dropped r2)))
    (is (= [] (:created r2)))
    (is (= 8 (:user-version r2)))
    (is (= ["organisations" "users"] (:tables st)))
    (is (= [] (:views st)))
    (core/close! d)))

(deftest-p migrate-v2-on-a-fresh-database-skips-the-alters
  (p/let [d (core/db)
          r (migrate/migrate! d defs-v2)
          cols (core/pragma d :table-info {:arg :users :rows? true})
          r2 (migrate/migrate! d defs-v2)]
    (is (= [] (:altered r)) "a fresh database gets the desired shape from CREATE TABLE")
    (is (= [:organisations :users :users-org-idx :users-mail-idx] (:created r)))
    (is (= [:orgs :users :users :user-emails :users-audit] (:existing r)) "skipped alters/drops report as existing")
    (is (= ["id" "mail" "org_id" "nickname"] (mapv :name cols)))
    (is (= [] (:created r2)))
    (core/close! d)))

(deftest-p migrate-rejects-a-def-renamed-away
  (p/let [d (core/db)
          [e] (rejection (migrate/migrate! d [{:create-table :a :columns [[:x :integer {}]]}
                                              {:alter-table :a :rename-to :b}]))
          st (migrate/status! d)]
    (is (= :drift (:reason (ex-data e))))
    (is (:missing-table (ex-data e)))
    (is (= [] (:tables st)) "rolled back")
    (core/close! d)))

(deftest-p migrate-refuses-a-newer-database
  (p/let [d (core/db)
          _ (migrate/migrate! d defs)
          [e] (rejection (migrate/migrate! d (subvec defs 0 2)))
          r (migrate/migrate! d (subvec defs 0 2) {:allow-newer? true})]
    (is (= :database-newer (:reason (ex-data e))))
    (is (= 5 (:user-version (ex-data e))))
    (is (= 2 (:user-version r)) ":allow-newer? true rewrites the fingerprint")
    (core/close! d)))

(deftest-p migrate-validates-defs-and-rolls-back-sqlite-errors
  (p/let [d (core/db)
          [e1] (rejection (migrate/migrate! d [{:create-table :t :columns [[:flag :boolean {}]]}]))
          [e2] (rejection (migrate/migrate! d [{:select :*}]))
          [e3] (rejection (migrate/migrate! d {:create-table :t :columns [[:a :integer]]}))
          [e4] (rejection (migrate/migrate! d [{:create-table :a :columns [[:x :integer {}]]}
                                             {:alter-table :nope :add-column [:y :integer {}]}]))
          st (migrate/status! d)]
    (is (= :invalid-defs (:reason (ex-data e1))))
    (is (= :sqlite-cljs/argument-error (:type (ex-data (:cause (ex-data e1))))) "the DDL validation error is the cause")
    (is (= :invalid-defs (:reason (ex-data e2))))
    (is (= :invalid-defs (:reason (ex-data e3))))
    (is (= :no-such-table (:reason (ex-data e4))))
    (is (= [] (:tables st)) "table a was rolled back with the failing alter")
    (is (= 0 (:user-version st)))
    (core/close! d)))

(deftest-p migrate-clears-statement-cache-and-logs
  (p/let [d (core/db)
          log (atom [])
          _ (q/query-all! d "SELECT 1 AS a")
          before (p/resolved (core/cache-size d))
          _ (migrate/migrate! d defs {:log #(swap! log conj %)})
          after (p/resolved (core/cache-size d))]
    (is (= 1 before))
    (is (= 0 after))
    (is (= 5 (count @log)))
    (is (= "migrate! create-table :orgs" (first @log)))
    (core/close! d)))

(deftest validate-is-pure
  (is (= [] (migrate/validate defs)))
  (is (= 0 (:index (first (migrate/validate [{:nope 1}])))))
  (is (= 1 (count (migrate/validate [(first defs) {:create-table :x :columns [[:b :bool {}]]}]))))
  (is (= nil (:index (first (migrate/validate "x"))))))
