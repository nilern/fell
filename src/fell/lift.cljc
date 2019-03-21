(ns fell.lift
  (:require [cats.core :refer [return bind]]
            [cats.protocols :refer [-extract]]
            [fell.core :refer [-extract request-eff append-handler]])
  (:import [fell.core Pure Impure]))

(defn lift [mv] (request-eff [::lift mv]))

(defn run-lift [ctx eff]
  (condp instance? eff
    Pure (return ctx (-extract eff))
    Impure (let [request (.-request eff)
                 cont (.-cont eff)
                 [tag mv] request]
             (case tag
               ::lift (bind mv (append-handler cont (partial run-lift ctx)))))))
