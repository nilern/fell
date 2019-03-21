(ns fell.delimc
  (:require [fell.core :refer [pure request-eff handle-relay]]))

;; Eff (Delimc | r) a -> Eff r a
(defn reset0 [eff]
  (handle-relay #(= (first %) ::shift0)
                pure
                (fn [[_ f] k] (f k))
                eff))

;; ((() -> Eff r a) -> Eff r a) -> Eff (Delimc | r) a
(defn shift0 [f] (request-eff [::shift0 f]))

