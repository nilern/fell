(ns fell.reader
  "Reader effect."
  (:require [fell.core :refer [pure request-eff handle-relay]]))

(defn make
  "Given the request keyword `tag`, return {:ask [[ask]]-for-`tag`, :run [[run-reader]]-for-`tag`}."
  [tag]
  {:ask (request-eff [tag])
   :run (fn [eff env]
          (handle-relay tag pure (fn [[_] cont] (cont env)) eff))})

(let [{:keys [ask run]} (make ::ask)]
  (def ask
    "An Eff that gets the Reader environment value."
    ask)

  (def run-reader
    "`(run-reader eff env)` runs the Reader effect in Eff `eff` using `env` as the environment value."
    run))
