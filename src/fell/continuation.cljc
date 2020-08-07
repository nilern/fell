(ns fell.continuation
  (:require [cats.core :refer [fmap]]))

(defn weave [k state handler]
  (fn [x] (handler (fmap (constantly (k x)) state))))
