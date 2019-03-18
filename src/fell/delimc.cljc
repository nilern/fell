(ns fell.delimc
  #?(:clj (:refer-clojure :exclude [send]))
  (:require [fell.core :refer [->Pure handle-relay send]]))

;; Eff (Delimc | r) a -> Eff r a
(defn reset0 [eff]
  (handle-relay #(= (first %) ::shift0)
                ->Pure
                (fn [[_ f] k] (f k))
                eff))

;; ((() -> Eff r a) -> Eff r a) -> Eff (Delimc | r) a
(defn shift0 [f]
  (send [::shift0 f]))

