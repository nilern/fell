(ns fell.queue
  #?(:clj (:import [clojure.lang PersistentQueue])))

(def empty-queue
  #?(:clj  PersistentQueue/EMPTY
     :cljs #queue []))

(defn singleton-queue [v]
  #?(:clj  (conj PersistentQueue/EMPTY v)
     :cljs #queue [v]))
