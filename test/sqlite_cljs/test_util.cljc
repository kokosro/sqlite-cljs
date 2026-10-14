(ns sqlite-cljs.test-util
  "Test helpers: the driver is synchronous, so promises are already settled;
   `deftest-p` runs a promise-returning body under cljs.test's async and
   reports an unexpected rejection as a failed assertion."
  #?(:cljs (:require [cljs.test :refer [async is]]
                     [promesa.core :as p])))

(defmacro deftest-p
  "(deftest-p name & body): body forms are chained with promesa.core/do so
   each may return a promise; a rejection fails the test."
  [name & body]
  `(cljs.test/deftest ~name
     (cljs.test/async done#
       (-> (promesa.core/do ~@body)
           (promesa.core/catch (fn [e#]
                                 (cljs.test/is (nil? e#) (str "unexpected rejection: " (ex-message e#) " " (pr-str (ex-data e#))))))
           (promesa.core/finally (fn [_# _#] (done#)))))))

#?(:cljs
   (defn rejection
     "Resolves to a one-element vector [error] holding the rejection error
      of `promise` ([nil] when it resolved). Boxed because promesa treats a
      bare js/Error value as a rejection; destructure with [e] in p/let."
     [promise]
     (-> promise
         (p/then (fn [_] [nil]))
         (p/catch (fn [e] [e])))))

#?(:cljs
   (defn settled-error
     "The error of an already-rejected promise (nil when resolved/pending).
      Usable synchronously inside transact! bodies."
     [promise]
     (when (p/rejected? promise) (p/extract promise))))

#?(:cljs
   (defn error-type
     "Resolves to the sqlite-cljs :type of the rejection (nil when resolved)."
     [promise]
     (p/then (rejection promise) #(some-> (first %) ex-data :type))))
