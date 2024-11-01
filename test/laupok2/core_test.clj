(ns laupok2.core-test
  (:require
   [clojure.test :refer :all]
   [clojure.test.check.generators :as gen]
   [clojure.test.check.properties :as prop]
   [clojure.test.check :refer [quick-check]]
   [laupok2.core :refer :all]))

(deftest add-remove-test
  (let [bag (-> empty-bag (add-to-bag 5) (add-to-bag 3) (add-to-bag 5))]
    (is (= 3 (:count (add-to-bag bag 5))))
    (is (= 1 (:count (remove-from-bag bag 5))))
    (is (nil? (find-count (remove-from-bag bag 3) 3)))))


(deftest filter-map-test
  (let [bag (-> empty-bag (add-to-bag 3) (add-to-bag 5) (add-to-bag 7))
        filtered-bag (filter-bag bag #(> % 4))
        mapped-bag (map-bag bag #(* % 2))]
    (is (= 5 (first (vals filtered-bag))))
    (is (= 6 (first (vals mapped-bag))))))

(deftest fold-test
  (let [bag (-> empty-bag (add-to-bag 1) (add-to-bag 2) (add-to-bag 3))]
    (is (= 6 (fold-left-bag bag + 0)))
    (is (= 6 (fold-right-bag bag + 0)))
    (is (= 12 (fold-left-bag bag * 2)))
    (is (= 12 (fold-right-bag bag * 2)))))


(deftest combine-test
  (let [bag1 (-> empty-bag (add-to-bag 1) (add-to-bag 2))
        bag2 (-> empty-bag (add-to-bag 3) (add-to-bag 4))]
    (is (= 4 (count-nodes (combine-bags bag1 bag2)))))) 



(def neutral-element-prop
  (prop/for-all [x (gen/vector (gen/choose 1 10))]
                (let [bag (reduce add-to-bag empty-bag x)]
                  (= bag (combine-bags bag empty-bag)))))

(deftest test-neutral-element
  (quick-check 100 neutral-element-prop))



(def associative-combine-prop
  (prop/for-all [x (gen/vector (gen/choose 1 10))
                 y (gen/vector (gen/choose 1 10))
                 z (gen/vector (gen/choose 1 10))]
                (let [bag1 (reduce add-to-bag empty-bag x)
                      bag2 (reduce add-to-bag empty-bag y)
                      bag3 (reduce add-to-bag empty-bag z)]
                  (= (combine-bags bag1 (combine-bags bag2 bag3))
                     (combine-bags (combine-bags bag1 bag2) bag3)))))

(deftest test-associativity
  (quick-check 100 associative-combine-prop))


(def filter-idempotent-prop
  (prop/for-all [x (gen/vector (gen/choose 1 10))]
                (let [pred #(> % 5)
                      bag (reduce add-to-bag empty-bag x)
                      filtered-once (filter-bag bag pred)
                      filtered-twice (filter-bag filtered-once pred)]
                  (= filtered-once filtered-twice))))


(deftest test-filter-idempotent
  (quick-check 100 filter-idempotent-prop))

