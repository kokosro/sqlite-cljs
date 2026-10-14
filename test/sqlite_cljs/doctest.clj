(ns sqlite-cljs.doctest
  "Macros of the README examples test.")

(defmacro claims
  "A vector of [source value] pairs, one per form: the form as it is
   written and what it evaluates to."
  [& forms]
  (mapv (fn [form] [(pr-str form) form]) forms))
