(ns fell.reader
  (:require [fell.core :refer [pure request-eff handle-relay]]))

(defn make [tag]
  {:ask (request-eff [tag])
   :run (fn [eff env]
          (handle-relay tag pure (fn [[_] cont] (cont env)) eff))})

(let [{:keys [ask run]} (make ::ask)]
  (def ask ask)
  (def run-reader run))
