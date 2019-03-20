(ns fell.util)

(defn singleton-queue [v]
  #?(:clj  (conj clojure.lang.PersistentQueue/EMPTY v)
     :cljs #queue [v]))
