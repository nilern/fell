(ns fell.state
  (:require [cats.core :refer [extract]]
            [fell.core :refer [pure impure bounce append-handler]]
            [fell.queue :refer [singleton-queue]])
  (:import [fell.core Pure Impure Bounce]))

(defn state-runner [label]
  (fn run-state [eff state]
    (condp instance? eff
      Pure (pure [(extract eff) state])
      Impure (let [[tag subtag state* :as request] (.-request eff)
                   make-cont (fn [state] (append-handler (.-cont eff) #(run-state % state)))]
               (if (= tag label)
                 (case subtag
                   :get (let [cont (make-cont state)]
                          (bounce #(cont state)))
                   :set (let [cont (make-cont state*)]
                          (bounce #(cont nil))))
                 (impure request (singleton-queue (make-cont state)))))
      Bounce (Bounce. (.-thunk eff) (.-cont eff) (conj (.-handlers eff) #(run-state % state))))))
