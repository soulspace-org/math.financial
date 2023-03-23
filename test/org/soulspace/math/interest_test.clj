(ns org.soulspace.math.interest-test
  (:require [clojure.test :refer :all]
            [org.soulspace.math.interest :as mi]
            ))

(deftest compile-test
  (testing "Testing compilation"
    (is (= 2 (+ 1 1)))))

(deftest percentage-test
  (testing "Testing percentage values"
    (are [x y] (== x y)
      0    (mi/percentage 0)
      1    (mi/percentage 0.01)
      10   (mi/percentage 0.1)
      100  (mi/percentage 1)
      1000 (mi/percentage 10)))) 

(deftest percent-test
  (testing "Testing percent values"
    (are [x y] (== x y)
      0    (mi/percent 0)
      0.01 (mi/percent 1)
      0.1  (mi/percent 10)
      1    (mi/percent 100)
      10   (mi/percent 1000))))

(deftest interest-test
      (testing "Testing interest function."
        (are [x y] (== x y)
          11 (mi/accumulate-by-interest 10 0.1)
          11 (mi/accumulate-by-interest 10 (mi/percent 10))))) 
