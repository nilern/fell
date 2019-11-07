(ns fell.delimc
  "Delimited continuations as an effect."
  (:require [fell.core :refer [pure request-eff handle-relay]]))

(defn make [tag]
  {:shift0 (fn [f] (request-eff [tag f]))
   :reset0 (fn [eff]
             (handle-relay tag pure (fn [[_ f] k] (f k)) eff))})

(let [{:keys [shift0 reset0]} (make ::shift0)]
  ;; ((() -> Eff r a) -> Eff r a) -> Eff (Delimc | r) a
  (def shift0 shift0)

  ;; Eff (Delimc | r) a -> Eff r a
  (def reset0 reset0))
