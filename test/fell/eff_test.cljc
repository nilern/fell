(ns fell.eff-test
  (:require [clojure.test :refer :all]

            [cats.core :refer [extract]]
            [cats.protocols :refer [-get-context]]
            [fell.queue :refer [empty-queue]]
            [fell.eff :refer :all])
  #?(:clj (:import [fell.eff Pure Impure])))

(deftest get-context-test
  (are [eff] (= (-get-context eff) context)
    (->Pure 23)
    (->Impure [::foo 23] empty-queue)))

(deftest extract-test
  (is (= (extract (->Pure 23)) 23)))

(deftest flat-map-test
  (testing "->Pure"
    (let [^Pure eff (-flat-map (->Pure 23) ->Pure)]
      (is (= (.-v eff) 23))))

  (testing "->Impure"
    (let [^Impure eff (-flat-map (->Impure [::foo 23] empty-queue) ->Pure)]
      (is (= (.-request eff) [::foo 23]))
      (is (= (.-cont eff) (conj empty-queue ->Pure))))))
