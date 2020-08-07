(ns fell.continuation
  "Continuation utilities."
  (:require [cats.core :refer [fmap]]))

(defn weave
  "Weave `state` and `handler` into the continuation fn `k`."
  [k state handler]
  (fn [x] (handler (fmap (constantly (k x)) state))))
