(ns fell.core-test
  (:require [clojure.test :refer :all]

            [cats.core :refer [return bind extract]]
            [cats.protocols :refer [-get-context]]
            [fell.core :refer :all]
            [fell.queue :refer [empty-queue singleton-queue]])
  (:import [fell.core Pure]))

(deftest get-context-test
  (are [eff] (= (-get-context eff) context)
    (pure 23)
    (request-eff [::foo 23])
    (bounce identity 23)))

(deftest extract-test
  (is (= (extract (pure 23)) 23)))

(deftest flat-map-test
  (testing "pure"
    (let [eff (-flat-map (pure 23) pure)]
      (is (= (.-v eff) 23))))

  (testing "impure"
    (let [eff (-flat-map (request-eff [::foo 23]) pure)]
      (is (= (.-request eff) [::foo 23]))
      (is (= (.-cont eff) (conj (singleton-queue pure) pure)))))

  (testing "bounce"
    (let [eff (-flat-map (bounce pure 23) pure)]
      (is (= (.-v eff) 23)))))

(deftest monad-test
  (testing "return"
    (let [eff (return context 23)]
      (is (instance? Pure eff))
      (is (= (.-v eff) 23))))

  (testing "bind"
    (testing "pure"
      (let [eff (bind (pure 23) return)]
        (is (= (.-v eff) 23))))

    (testing "impure"
      (let [eff (bind (request-eff [::foo 23]) return)]
        (is (= (.-request eff) [::foo 23]))))

    (testing "bounce"
      (let [eff (bind (bounce pure 23) return)]
        (is (= (.-v eff) 23))))))

(deftest default-queue?-test
  (is (default-queue? (singleton-queue pure)))
  (is (not (default-queue? empty-queue)))
  (is (not (default-queue? (conj (singleton-queue pure) pure))))
  (is (not (default-queue? (singleton-queue return)))))

(deftest request-eff-test
  (let [eff (request-eff [::foo 23])]
    (is (= (.-request eff) [::foo 23]))
    (is (default-queue? (.-cont eff)))))

(deftest run-test
  (testing "pure"
    (is (= (run (pure 23)) 23)))

  (testing "impure"
    (is (thrown? RuntimeException (run (request-eff [::foo 23])))))

  (testing "bounce"
    (is (= (run (bounce pure 23)) 23))
    (is (thrown? RuntimeException (run (bounce request-eff [::foo 23]))))))
