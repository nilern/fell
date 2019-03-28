(ns fell.lift
  (:require [cats.core :refer [return bind extract]]
            [fell.core :refer [request-eff append-handler eff-trampoline
                               #?@(:cljs [Pure Impure Bounce])]])
  #?(:clj (:import [fell.core Pure Impure Bounce])))

(defn lift [mv] (request-eff [::lift mv]))

(defn run-lift [ctx eff]
  (condp instance? eff
    Pure (return ctx (extract eff))
    Impure (let [request (.-request eff)
                 cont (.-cont eff)
                 [tag mv] request]
             (case tag
               ::lift (bind mv (append-handler cont (partial run-lift ctx)))))
    Bounce (recur ctx (eff-trampoline eff))))
