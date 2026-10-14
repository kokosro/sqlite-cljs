(ns sqlite-cljs.dev
  "Node-script entry point for interactive development."
  (:require [sqlite-cljs.util :as util]))

(defn main [& _]
  (println "sqlite-cljs dev runtime loaded. Attach with: npx shadow-cljs cljs-repl dev")
  (println "identifier quoting sanity:" (util/sql-ident :user-name) (util/sql-ident "we\"ird"))
  (js/setInterval #(println "…dev heartbeat") 60000))
