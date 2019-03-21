(ns fell.state
  (:require [cats.core :refer [extract]]
            [fell.core :refer [pure impure append-handler]]
            [fell.queue :refer [singleton-queue]])
  (:import [fell.core Pure Impure]))

(defn state-runner [label]
  (fn run-state [mv domain-state]
    (condp instance? mv
      Pure (pure [(extract mv) domain-state])
      Impure (let [[tag subtag state* :as request] (.-request mv)
                   make-cont (fn [domain-state]
                               (append-handler (.-cont mv) #(run-state % domain-state)))]
               (if (= tag label)
                 (case subtag
                   :get ((make-cont domain-state) domain-state)
                   :set ((make-cont state*) nil))
                 (impure request (singleton-queue (make-cont domain-state))))))))
