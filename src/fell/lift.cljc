(ns fell.lift
  (:require [cats.core :refer [return bind extract]]
            [fell.core :refer [request-eff append-handler bounce eff-trampoline]])
  (:import [fell.core Pure Impure Bounce]))

(defn lift [mv] (request-eff [::lift mv]))

(defn run-lift [ctx eff]
  (condp instance? eff
    Pure (return ctx (extract eff))
    Impure (let [request (.-request eff)
                 cont (.-cont eff)
                 [tag mv] request]
             (case tag
               ::lift (bounce bind mv (append-handler cont (partial run-lift ctx)))))
    Bounce (recur ctx (eff-trampoline eff))))
