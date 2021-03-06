(ns fell.core-test
  (:require [clojure.test :refer :all]

            [cats.core :refer [return bind]]
            [fell.eff :refer [context]]
            [fell.core :refer :all])
  (:import [fell.eff Pure Impure]))

(deftest monad-test
  (testing "return"
    (let [eff (return context 23)]
      (is (instance? Pure eff))
      (is (= (.-v ^Pure eff) 23))))

  (testing "bind"
    (testing "pure"
      (let [eff (bind (pure 23) return)]
        (is (= (.-v ^Pure eff) 23))))

    (testing "impure"
      (let [eff (bind (request-eff [::foo 23]) return)]
        (is (= (.-request ^Impure eff) [::foo 23]))))))

(deftest request-eff-test
  (let [eff (request-eff [::foo 23])]
    (is (= (.-request eff) [::foo 23]))))

(deftest run-test
  (testing "pure"
    (is (= (run (pure 23)) 23)))

  (testing "impure"
    (is (thrown? RuntimeException (run (request-eff [::foo 23]))))))
