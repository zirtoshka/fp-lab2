(ns laupok2.core-test
  (:require [clojure.test :refer :all]
            [laupok2.core :refer :all]))



(deftest add-node-test
  (testing "add elements to tree"
    (let [tree (add-node nil 5)
          tree2 (add-node tree 3)
          tree3 (add-node tree2 5)]
      (is (= (:value tree)5))
      (is(= (:count tree3)2))
      (is (= (:value (:left tree2))3)))))
