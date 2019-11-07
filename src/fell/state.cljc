(ns fell.state
  "State effect."
  (:refer-clojure :exclude [get set])
  (:require [cats.core :refer [extract]]
            [fell.eff :refer [#?@(:cljs [Pure Impure])]]
            [fell.queue :refer [append-handler]]
            [fell.core :refer [pure impure request-eff #?@(:cljs [Pure Impure])]]
            [fell.queue :refer [singleton-queue]])
  #?(:clj (:import [fell.eff Pure Impure])))

(defn make
  "Given the request keyword `tag`, return
  {:get [[get]]-for-`tag`, :set [[set]]-for-`tag`, :run [[run-state]]-for-`tag`}."
  [tag]
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
  (def get
    "An Eff that gets the State state value."
    get)

  (def set
    "`(set state*)` is an Eff that sets the State state value to `value*`."
    set)

  (def run-state
    "`(run-state eff state)` runs the State effect in Eff `eff` using `state` as the initial state value."
    run))
