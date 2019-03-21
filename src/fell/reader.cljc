(ns fell.reader

  (:require [fell.core :refer [pure request-eff handle-relay]]))

(def ask (request-eff [::ask]))

(defn run-reader [mv env]
  (handle-relay #(= (first %) ::ask)
                pure
                (fn [[_] cont] (cont env)) mv))
