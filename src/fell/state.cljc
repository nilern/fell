(ns fell.state
  (:refer-clojure :exclude [get set])
  (:require [cats.core :refer [extract]]
            [fell.core :refer [pure impure request-eff append-handler
                               #?@(:cljs [Pure Impure])]]
            [fell.queue :refer [singleton-queue]])
  #?(:clj (:import [fell.core Pure Impure])))

(defn make [tag]
  {:get (request-eff [tag :get])
   :set (fn [state*] (request-eff [tag :set state*]))
   :run (fn run-state [eff state]
          (condp instance? eff
            Pure (pure [(extract eff) state])
            Impure (let [[tag* subtag state* :as request] (.-request eff)
                         make-cont (fn [state] (append-handler (.-cont eff) #(run-state % state)))]
                     (if (= tag* tag)
                       (case subtag
                         :get (let [cont (make-cont state)]
                                (cont state))
                         :set (let [cont (make-cont state*)]
                                (cont nil)))
                       (impure request (singleton-queue (make-cont state)))))))})

(let [{:keys [get set run]} (make ::state)]
  (def get get)
  (def set set)
  (def run-state run))
