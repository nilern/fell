(ns fell.delimc
  #?(:clj (:refer-clojure :exclude [send]))
  (:require [fell.core :refer [->Pure handle-relay send]]))

(defn reset0 [eff]
  (handle-relay #(= (first %) ::shift0)
                ->Pure
                (fn [[_ f] k] (f k))
                eff))

(defn shift0 [f]
  (send [::shift0 f]))

