# sqlite-cljs

[SQLite](https://sqlite.org) from ClojureScript, with schemas and queries as data.

`sqlite-cljs` is a client for SQLite on Node.js, built on the
`better-sqlite3` driver. Schemas
and queries are ClojureScript maps and vectors, and the library renders the SQL from
them: every value becomes a bound parameter and every identifier is quoted, so no SQL is
put together from strings. A schema is applied idempotently, rows come back as
kebab-case keyword maps, and every operation returns a
promise, settled already since the driver is synchronous. The database is a file or lives in
memory; there is no server and no network. `better-sqlite3` is the only runtime npm
dependency.

- [Requirements](#requirements)
- [Quick start](#quick-start)
- [Scenarios](#scenarios)
- [Error handling](#error-handling)
- [Features](#features)
- [Limits and settings](#limits-and-settings)
- [Documentation](#documentation)
- [Layout](#layout)
- [Development](#development)

## Requirements

You need Node.js 22 or later and a JDK (for shadow-cljs). `better-sqlite3` itself is an
npm package that your project installs; it ships prebuilt binaries for the common
platforms.

## Quick start

Open a database in memory, create a table, insert four rows, query them and update one.

**1. Get the code.**

```sh

mkdir -p my-app/src/quickstart && cd my-app
npm init -y
npm install better-sqlite3
npm install --save-dev shadow-cljs
```

**2. Declare the dependency.** With the Clojure CLI, `deps.edn` names the checkout and
`shadow-cljs.edn` defers to it:

```clojure
;; deps.edn
{:paths ["src"]
 :deps {kokosro/sqlite-cljs {:local/root "../sqlite-cljs"}
        thheller/shadow-cljs {:mvn/version "2.28.23"}}}
```

```clojure
;; shadow-cljs.edn
{:deps true
 :builds {:app {:target :node-script
                :main quickstart.core/main
                :output-to "out/quickstart.js"}}}
```

Without the Clojure CLI, leave `deps.edn` out and put the sources on the path of
shadow-cljs:

```clojure
;; shadow-cljs.edn
{:source-paths ["src" "../sqlite-cljs/src"]
 :dependencies [[funcool/promesa "12.0.1"]]
 :builds {:app {:target :node-script
                :main quickstart.core/main
                :output-to "out/quickstart.js"}}}
```

**3. Write the program.** `src/quickstart/core.cljs`:

```clojure
(ns quickstart.core
  (:require [promesa.core :as p]
            [sqlite-cljs.core :as sqlite]
            [sqlite-cljs.dsl :as dsl]
            [sqlite-cljs.migrate :as migrate]
            [sqlite-cljs.query :as q]))

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
```

**4. Run it.**

```sh
npx shadow-cljs compile app
node out/quickstart.js
```

It prints:

```clojure
:created [:users :users-score-idx]
:inserted {:changes 4, :last-insert-rowid 4}
:sql {:sql "SELECT \"id\", \"name\", \"score\", \"active\" FROM \"users\" WHERE (\"active\" = ? AND \"score\" >= ?) ORDER BY \"score\" DESC", :params [true 50]}
:rows [{:id 1, :name "Ada", :score 620, :active true} {:id 3, :name "Carol", :score 75, :active true}]
:one {:id 2, :email "bob@example.com"}
:total 4
:updated [{:name "Bob", :score 15}]
```

| Line | What it is |
|------|------------|
| `:created` | what `migrate!` created: the table and its index. A second run creates nothing |
| `:inserted` | four rows went in with one statement |
| `:sql` | the query `high-scores` as it is sent: SQL with a `?` for every value, and the values beside it |
| `:rows` | the rows, as maps with kebab-case keys. `:active` is stored as the integer 1; `:coerce` reads it as a boolean |
| `:one` | one row. It is found by an email in capitals because the column is declared `:collate :nocase` |
| `:total` | one value: the first column of the first row |
| `:updated` | the rows an update with `:returning` gives back |

For a database in a file, give `sqlite/db` a path: `(sqlite/db "app.db")`.

The checkout runs the same program without a project of your own:

```sh
cd sqlite-cljs
npm install
npx shadow-cljs compile examples && node target/examples.js
```

## Scenarios

The examples work on the database of the quick start, with `schema` and the four users
in it, and use these namespaces:

```clojure
(ns my.app
  (:require [promesa.core :as p]
            [sqlite-cljs.core :as sqlite]
            [sqlite-cljs.migrate :as migrate]
            [sqlite-cljs.query :as q]
            [sqlite-cljs.util :as util]))
```

### A schema that grows

`migrate!` takes the definitions in order and brings the database up to them: it creates
what is absent and leaves what exists alone, inside one transaction. A table definition
describes the shape you want. An `:alter-table` definition beside it brings an existing
database there; a new database gets the shape at once and skips the alter.

```clojure
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
```

```clojure
(upgrade! db schema)
;; an empty database; resolves to
{:created [:users :users-score-idx]
 :existing []
 :altered []
 :user-version 1
 :previous-user-version 0}

(upgrade! db schema-2)
;; the same database, later; the rows of users are kept
{:created [:orders :orders-user-idx]
 :existing [:users :users-score-idx]
 :altered [:users]
 :user-version 3
 :previous-user-version 1}

(upgrade! db schema-2)
;; once more: nothing to do
{:created []
 :existing [:users :users-score-idx :users :orders :orders-user-idx]
 :altered []
 :user-version 3
 :previous-user-version 3}
```

Nothing is altered on its own account. A table that lacks a declared column, or has it
with another type, is reported as drift and the whole run is rolled back. So are
definitions that are older than the database:

```clojure
(migrate/migrate! db [(first schema-2)])
;; on a database with `schema`: the column is declared, its alter is not. Rejects with
{:type :sqlite-cljs/migration-error
 :reason :drift
 :table :users
 :missing ["country"]
 :type-mismatch []}

(migrate/migrate! db schema)
;; on a database with `schema-2`. Rejects with
{:type :sqlite-cljs/migration-error
 :reason :database-newer
 :user-version 3
 :defs 1}
```

### All or nothing

`transact!` runs a function inside a transaction. It commits when the function returns
and rolls back when it throws. The driver is synchronous, so the function is too: the
promises inside it are settled already and are read with `@`.

```clojure
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
```

```clojure
(transfer! db "ada@example.com" "bob@example.com" 100)
;; resolves to
{"ada@example.com" 520 "bob@example.com" 115}

(try-transfer! db "bob@example.com" "ada@example.com" 500)
;; resolves to
{:message "not enough points"
 :data {:from "bob@example.com" :points 500}
 :scores [{:email "ada@example.com" :score 520} {:email "bob@example.com" :score 115}]}
```

### Insert or update

```clojure
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
```

```clojure
(save-user! db {:email "eve@example.com" :name "Eve" :score 1})
;; resolves to
{:id 5 :email "eve@example.com" :name "Eve" :score 1}

(save-user! db {:email "EVE@example.com" :name "Eve Adams" :score 40})
;; the same user: resolves to
{:id 5 :email "eve@example.com" :name "Eve Adams" :score 40}
```

## Error handling

A function that returns a promise rejects and does not throw; `sqlite/db` and the pure
renderers (`dsl/render`, `ddl/render`) throw. Either way the error is an `ex-info` whose
`ex-data` carries a `:type`:

| `:type` | Meaning |
|---------|---------|
| `:sqlite-cljs/sqlite-error` | SQLite raised it. `:code` is its extended result code as a string, `:sql` the statement |
| `:sqlite-cljs/argument-error` | an argument or a value was refused before any SQL ran (`:reason`) |
| `:sqlite-cljs/dsl-error` | a map that is not a statement the renderer takes (`:form`) |
| `:sqlite-cljs/config-error` | the database could not be opened as asked (`:reason`) |
| `:sqlite-cljs/migration-error` | a schema that cannot be applied (`:reason`) |

`util/error-data`, `util/error-type`, `util/sqlite-code`, `util/constraint-error?` and
`util/busy-error?` read an error. An insert with every failure turned into data:

```clojure
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
```

```clojure
(add-user! db {:email "eve@example.com" :name "Eve"})
;; resolves to
{:id 5}

(add-user! db {:email "ADA@example.com" :name "Ada again"})
;; the email is taken: resolves to
{:failed {:type :sqlite-cljs/sqlite-error
          :code "SQLITE_CONSTRAINT_UNIQUE"
          :message "UNIQUE constraint failed: users.email"}}
```

An order of a user who does not exist, a text in an integer column of a `STRICT` table,
and an update that names no rows:

```clojure
(q/execute! db {:insert-into :orders :values [{:user-id 99 :amount 5 :placed-at 0}]})
;; rejects with
{:type :sqlite-cljs/sqlite-error
 :code "SQLITE_CONSTRAINT_FOREIGNKEY"
 :message "FOREIGN KEY constraint failed"}

(q/execute! db {:update :users :set {:score "many"} :where [:= :name "Ada"]})
;; rejects with
{:type :sqlite-cljs/sqlite-error
 :code "SQLITE_CONSTRAINT_DATATYPE"
 :message "cannot store TEXT value in INTEGER column users.score"}

(q/execute! db {:update :users :set {:score 0}})
;; rejects before any SQL runs, with
{:type :sqlite-cljs/dsl-error :form {:update :users :set {:score 0}}}
```

## Features

`db` is a database in memory, as `(sqlite/db {:memory? true})` opens it.

### Queries as data: `sqlite-cljs.dsl`

A map is a statement, a vector that begins with a keyword is an operator or a function
call, a keyword is an identifier and everything else is a value. `dsl/render` is pure: it
turns the map into SQL and parameters and touches no database.

```clojure
(dsl/render {:select [:id] :from :users :where [:= :name "x'; DROP TABLE users; --"]})
;; => {:sql "SELECT \"id\" FROM \"users\" WHERE \"name\" = ?", :params ["x'; DROP TABLE users; --"]}

(dsl/render {:select [:u/name [[:count :*] :orders]] :from [[:users :u]] :left-join [[:orders :o] [:= :o/user-id :u/id]] :group-by [:u/name]})
;; => {:sql "SELECT \"u\".\"name\", count(*) AS \"orders\" FROM \"users\" AS \"u\" LEFT JOIN \"orders\" AS \"o\" ON \"o\".\"user_id\" = \"u\".\"id\" GROUP BY \"u\".\"name\"", :params []}

(dsl/render {:select [:*] :from :users :where [:in :id [1 2 3]]})
;; => {:sql "SELECT * FROM \"users\" WHERE \"id\" IN (?, ?, ?)", :params [1 2 3]}

(dsl/render {:select [:*] :from :users :where [:= :country nil]})
;; => {:sql "SELECT * FROM \"users\" WHERE \"country\" IS NULL", :params []}

(dsl/render {:delete-from :users :where [:< :score 0]})
;; => {:sql "DELETE FROM \"users\" WHERE \"score\" < ?", :params [0]}

(count (:params (dsl/render {:select [:*] :from :users :where [:in :id (range 1000)]})))
;; => 1
```

Select, insert, upsert, update and delete; joins, subqueries, common table expressions,
unions, window functions, `:returning`. An update or a delete without `:where` is
refused unless it says `:all? true`.

### Schemas as data: `sqlite-cljs.ddl`

Tables, indexes, views, triggers, alters and drops. `ddl/render` is pure as well.

```clojure
(:statements (ddl/render {:create-table :tags :columns [[:id :integer {:primary-key? true}] [:label :text {:not-null? true}]]}))
;; => ["CREATE TABLE IF NOT EXISTS \"tags\" (\"id\" INTEGER PRIMARY KEY, \"label\" TEXT NOT NULL) STRICT"]

(:statements (ddl/render {:create-index :tags-label-idx :on :tags :columns [:label] :unique? true}))
;; => ["CREATE UNIQUE INDEX IF NOT EXISTS \"tags_label_idx\" ON \"tags\" (\"label\")"]
```

Column types are `:integer` `:real` `:text` `:blob` `:any`. A boolean is stored as the
integer 0 or 1 and a point in time as an integer of milliseconds since the epoch;
`:coerce` reads them back as `:boolean` and `:date`.

### Identifiers are quoted: `sqlite-cljs.util`

```clojure
(util/sql-ident :user-name)
;; => "\"user_name\""

(util/sql-ident "we\"ird")
;; => "\"we\"\"ird\""
```

### Running statements: `sqlite-cljs.query`

| Function | Result |
|----------|--------|
| `query-all!` | the rows, a vector of maps |
| `query-one!` | the first row, or `nil` |
| `query-value!` | the first column of the first row |
| `execute!` | `{:changes :last-insert-rowid}`, or the rows of `:returning` |
| `execute-all!` | several statements in one transaction |
| `exec!` | SQL without parameters: a script, or definitions |
| `transact!` | a function inside a transaction |

Each takes a map of the DSL, a vector `[sql & params]`, a map `{:sql :params}` or a
string. `{:coerce {:active :boolean}}` as the last argument converts columns on the way
out: `:boolean` `:date` `:number` `:bigint` `:json` `:keyword`, or a function.

### Schema migration: `sqlite-cljs.migrate`

`migrate!` of the section above, `validate` to check definitions without a database, and
`status!` for the tables, views, indexes and triggers a database has.

### Connections: `sqlite-cljs.core`

```clojure
(:memory? db)
;; => true

(sqlite/open? db)
;; => true
```

`db`, `close!`, `pragma` and `pragma!`, `sqlite-version`, `driver-info`, and
`define-function!` to call a ClojureScript function from SQL.

The driver is synchronous: a statement runs to its end before the function returns, and
the promise it returns is settled already. The promises keep the call sites as they are
should a driver be asynchronous.

## Limits and settings

`sqlite/db` opens a connection with the settings below, which differ from what SQLite
does when nothing is said. Each is an option of `sqlite/db`, and the connection reports
what is in force under `:effective`:

```clojure
(:effective db)
;; => {:journal-mode "memory", :synchronous 2, :foreign-keys? true, :busy-timeout-ms 5000, :trusted-schema? false, :safe-integers? false}
```

| Option | Set | Absent |
|--------|-----|--------|
| `:foreign-keys?` | `false` leaves foreign keys unchecked, as SQLite does by itself | foreign keys are enforced |
| `:timeout-ms` | how long a statement waits for a lock that another connection holds, in milliseconds; `0` does not wait | 5000 |
| `:journal-mode` | `:delete` `:truncate` `:persist` `:memory` `:off` `:wal` | `:wal` for a file that is opened for writing; a database in memory has none |
| `:require-journal-mode?` | `false` accepts the journal mode SQLite fell back to | a journal mode that could not be set is an error |
| `:synchronous` | `:off` `:normal` `:full` `:extra` | not set: the value of the driver stays |
| `:trusted-schema?` | `true` trusts the functions a schema calls | not trusted |
| `:safe-integers?` | `true` reads every INTEGER as a BigInt | a number |
| `:cache-size` | how many prepared statements a connection keeps | 128 |

```clojure
(sqlite/db {:path "app.db"
            :journal-mode :delete
            :synchronous :full
            :timeout-ms 0
            :foreign-keys? false})
```

Outside `sqlite/db` there are four more:

| Option | Where | Absent |
|--------|-------|--------|
| `:strict?` | a `:create-table` definition | `true`: the table is `STRICT` and refuses a value of another type than its column |
| `:if-not-exists?` | a `:create-table`, `:create-index`, `:create-view` definition | `true` |
| the mode | `(q/transact! db :deferred f)`, `:immediate`, `:exclusive` | `:immediate`: the write lock is taken when the transaction begins |
| `:in-json-threshold` | a query | a list of more than 500 values in `[:in ...]` travels as one JSON parameter |

An insert of more rows than fit in one statement (SQLite takes 32766 parameters) is
split into several statements inside one transaction.

## Documentation

- [`docs/sqlite-cljs.org`](docs/sqlite-cljs.org) is the manual: the db value and its
  pragmas, the DDL and the query DSL with every operator, `migrate!`, running queries,
  transactions, how values are bound and read, error handling, and an index of every
  public function by namespace.
- [`docs/sqlite-api-inventory.org`](docs/sqlite-api-inventory.org) maps the grammar of
  SQLite and the API of the driver to the data the library takes.

## Layout

Every public function carries a docstring; the namespaces are `sqlite-cljs.core`,
`sqlite-cljs.query`, `sqlite-cljs.dsl`, `sqlite-cljs.ddl`, `sqlite-cljs.migrate` and
`sqlite-cljs.util`.

```
src/sqlite_cljs/
  core.cljs      the driver binding: db and close!, pragmas, value binding, the statement cache, error mapping
  query.cljs     running statements over core: the query designators, rows and change counts, transact!
  migrate.cljs   migrate!, status! and validate over core and ddl
  dsl.cljs       the query renderer: a statement map to {:sql :params} (pure)
  ddl.cljs       the schema renderer: a definition map to CREATE statements, STRICT validation (pure)
  util.cljs      kebab/snake translation, identifier quoting, row conversion, error constructors, version compare (pure)
dev/sqlite_cljs/
  examples.cljs   the examples of this README
  smoke.cljs      the smoke: a database file in a temp directory beside one in memory
  dev.cljs        the entry point of the REPL build
dev/
  probe-bs3.js, probe-edge.js   probes of the driver, run by hand with node
test/sqlite_cljs/
  util_test.cljs, dsl_test.cljs, ddl_test.cljs, core_test.cljs, query_test.cljs, migrate_test.cljs   the suite, one per namespace
  examples_test.cljs   the examples of this README, run: it holds the README to the code
  test_util.cljc       helpers: a test over a promise, settled already since the driver is synchronous
  doctest.clj          macros of the examples test
test/publicity_gate.sh   the documentation gate that run-tests.sh runs first
docs/
  sqlite-cljs.org            the manual
  sqlite-api-inventory.org   the API inventory
```

## Development

```sh
./run-tests.sh
```

runs the documentation gate and the test suite. It uses no network: the databases are in
memory or files in temp directories that are removed afterwards. The examples of this
README are part of it: the suite fails when the README shows code or values that differ
from what runs.

```sh
npx shadow-cljs compile examples && node target/examples.js
```

runs the quick start.

```sh
npx shadow-cljs compile smoke && node target/smoke.js
```

drives the library against a database file in a temp directory, section by section.

The library targets Node.js: the driver is a native module. There is no browser build.

## License

MIT
