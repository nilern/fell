(ns fell.fiber
  (:require [fell.core :refer [->Pure run]]
            [fell.delimc :refer [reset0 shift0]]))

(def suspend shift0)

(defn schedule [eff]
  #?(:clj (future (run (reset0 eff)))
     :cljs (.requestIdleCallback js/window (fn [_] (run (reset0 eff)))))
  nil)

(def spawn schedule)

(def yield (suspend (fn [k] (->Pure (schedule (k nil))))))

