(ns fell.fiber
  (:require [fell.core :refer [->Pure run]]
            [fell.delimc :refer [reset0 shift0]]))

;; ((() -> Eff r a) -> Eff r a) -> Eff (Delimc | r) a
(def suspend shift0)

;; Eff Delimc a -> ()
(defn schedule [eff]
  #?(:clj (future (run (reset0 eff)))
     :cljs (.requestIdleCallback js/window (fn [_] (run (reset0 eff)))))
  nil)

;; Eff Delimc a -> Eff r' ()
(def spawn schedule)

;; Eff (Delimc | r) ()
(def yield (suspend (fn [k] (->Pure (schedule (k nil))))))
