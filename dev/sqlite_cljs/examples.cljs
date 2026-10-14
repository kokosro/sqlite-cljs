(ns sqlite-cljs.examples
  "The examples of the README, as functions.

   `main` is the quick start: it opens a database in memory, creates a
   table, inserts, queries and updates, and prints the rows. No file and no
   network:

     npx shadow-cljs compile examples && node target/examples.js

   Every function takes a db value, so it runs on a database in memory and
   on a file alike.

   The code between the README:begin / README:end marks is the code the
   README shows; test/sqlite_cljs/examples_test.cljs runs it and holds the
   two together."
  (:require [promesa.core :as p]
            [sqlite-cljs.core :as sqlite]
            [sqlite-cljs.dsl :as dsl]
            [sqlite-cljs.migrate :as migrate]
            [sqlite-cljs.query :as q]
            [sqlite-cljs.util :as util]))

;; ---------------------------------------------------------------------------
;; Quick start
;; ---------------------------------------------------------------------------

;; README:begin quick-start
(def schema
  [{:create-table :users
    :columns [[:id :integer {:primary-key? true}]
              [:email :text {:not-null? true :unique? true :collate :nocase}]
              [:name :text {:not-null? true}]
              [:score :integer {:not-null? true :default 0}]
              [:active :integer {:not-null? true :default true}]]
    :indexes [[:users-score-idx [[:score :desc]]]]}])

(def high-scores
  {:select [:id :name :score :active]
   :from :users
   :where [:and [:= :active true] [:>= :score 50]]
   :order-by [[:score :desc]]})

(defn quick-start
  "Promise of everything the quick start reads, as a map. `db` is an open
   and empty database."
  [db]
  (p/let [migrated (migrate/migrate! db schema)
          inserted (q/execute! db {:insert-into :users
                                   :values [{:email "ada@example.com" :name "Ada" :score 620 :active true}
                                            {:email "bob@example.com" :name "Bob" :score 10 :active true}
                                            {:email "carol@example.com" :name "Carol" :score 75 :active true}
                                            {:email "dan@example.com" :name "Dan" :score 90 :active false}]})
          rows     (q/query-all! db high-scores {:coerce {:active :boolean}})
          one      (q/query-one! db {:select [:id :email]
                                     :from :users
                                     :where [:= :email "BOB@EXAMPLE.COM"]})
          total    (q/query-value! db {:select [[[:count :*] :n]] :from :users})
          updated  (q/execute! db {:update :users
                                   :set {:score [:+ :score 5]}
                                   :where [:= :name "Bob"]
                                   :returning [:name :score]})]
    {:created (:created migrated)
     :inserted inserted
     :sql (dsl/render high-scores)
     :rows rows
     :one one
     :total total
     :updated updated}))

(def printed
  "The keys of the quick start in the order `main` prints them."
  [:created :inserted :sql :rows :one :total :updated])

(defn main [& _]
  (let [db (sqlite/db {:memory? true})]
    (-> (quick-start db)
        (p/then (fn [result]
                  (doseq [k printed]
                    (println k (pr-str (get result k))))))
        (p/catch (fn [e]
                   (println "failed:" (ex-message e))
                   (set! (.-exitCode js/process) 1)))
        (p/finally (fn [_ _] (sqlite/close! db))))))
;; README:end quick-start

;; ---------------------------------------------------------------------------
;; A schema that grows
;; ---------------------------------------------------------------------------

;; README:begin schema
(def schema-2
  "The schema of the quick start, later: users have a country, and there
   are orders."
  [{:create-table :users
    :columns [[:id :integer {:primary-key? true}]
              [:email :text {:not-null? true :unique? true :collate :nocase}]
              [:name :text {:not-null? true}]
              [:score :integer {:not-null? true :default 0}]
              [:active :integer {:not-null? true :default true}]
              [:country :text {}]]
    :indexes [[:users-score-idx [[:score :desc]]]]}
   {:alter-table :users :add-column [:country :text {}]}
   {:create-table :orders
    :columns [[:id :integer {:primary-key? true}]
              [:user-id :integer {:not-null? true
                                  :references {:table :users :column :id :on-delete :cascade}}]
              [:amount :integer {:not-null? true :check [:> :amount 0]}]
              [:placed-at :integer {:not-null? true}]]
    :indexes [[:orders-user-idx [:user-id]]]}])

(defn upgrade!
  "Brings `db` to `defs`. Promise of what was done."
  [db defs]
  (p/let [report (migrate/migrate! db defs)]
    (select-keys report [:created :existing :altered :user-version :previous-user-version])))
;; README:end schema

;; ---------------------------------------------------------------------------
;; All or nothing
;; ---------------------------------------------------------------------------

;; README:begin transfer
(defn transfer!
  "Moves `points` of score from the user `from` to the user `to`, both
   named by email. Both updates are applied or neither is. Promise of the
   two scores."
  [db from to points]
  (q/transact! db
    (fn [tx]
      (let [score-of (fn [email]
                       @(q/query-value! tx {:select [:score] :from :users
                                            :where [:= :email email]}))
            move     (fn [email delta]
                       @(q/execute! tx {:update :users
                                        :set {:score [:+ :score delta]}
                                        :where [:= :email email]}))]
        (move from (- points))
        (move to points)
        (when (neg? (score-of from))
          (throw (ex-info "not enough points" {:from from :points points})))
        {from (score-of from) to (score-of to)}))))

(defn try-transfer!
  "Promise of the scores after the transfer, or of why it was refused and
   the scores as they are. Never rejects."
  [db from to points]
  (-> (transfer! db from to points)
      (p/catch (fn [e]
                 (p/let [scores (q/query-all! db {:select [:email :score]
                                                  :from :users
                                                  :where [:in :email [from to]]
                                                  :order-by [:email]})]
                   {:message (ex-message e)
                    :data (ex-data e)
                    :scores scores})))))
;; README:end transfer

;; ---------------------------------------------------------------------------
;; Insert or update
;; ---------------------------------------------------------------------------

;; README:begin upsert
(defn save-user!
  "Inserts the user, or updates name and score of the user who has the
   email already. Promise of the row as it is stored."
  [db user]
  (p/let [rows (q/execute! db {:insert-into :users
                               :values [user]
                               :on-conflict [:email]
                               :do-update-set [:name :score]
                               :returning [:id :email :name :score]})]
    (first rows)))
;; README:end upsert

;; ---------------------------------------------------------------------------
;; Errors
;; ---------------------------------------------------------------------------

;; README:begin errors
(defn explain
  "What went wrong, as data."
  [e]
  (select-keys (util/error-data e) [:type :code :reason :message]))

(defn add-user!
  "Promise of {:id n} for the user inserted, or of {:failed data}. Never
   rejects."
  [db user]
  (-> (q/execute! db {:insert-into :users :values [user]})
      (p/then (fn [{:keys [last-insert-rowid]}] {:id last-insert-rowid}))
      (p/catch (fn [e] {:failed (explain e)}))))
;; README:end errors
