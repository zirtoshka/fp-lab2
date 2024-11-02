(ns laupok2.core-test
  (:require
   [clojure.test :refer [deftest is]]
   [clojure.test.check.generators :as gen]
   [clojure.test.check.properties :as prop]
   [clojure.test.check :refer [quick-check]]
   [laupok2.core :refer [add-to-bag combine-bags count-nodes empty-bag filter-bag find-count fold-left-bag fold-right-bag map-bag remove-from-bag]]))

(deftest add-remove-test
  (let [bag (-> empty-bag (add-to-bag 5) (add-to-bag 3) (add-to-bag 5))]
    (is (= 3 (:count (add-to-bag bag 5))))
    (is (= 1 (:count (remove-from-bag bag 5))))
    (is (= 0 (find-count (remove-from-bag bag 3) 3)))))

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


(defrecord CustomElement [key])

(defn custom-comparator [a b]
  (let [len-diff (compare (count (:key a)) (count (:key b)))]
    (if (zero? len-diff)
      (compare (:key a) (:key b))
      len-diff)))



(deftest test-add-to-bag-custom-comparator
  (let [elem1 (->CustomElement "abc")
        elem2 (->CustomElement "abcd")
        elem3 (->CustomElement "ab")
        bag1 (-> empty-bag
                 (add-to-bag (->CustomElement elem1) custom-comparator))
        bag2 (add-to-bag bag1 elem2 custom-comparator)
        bag3 (add-to-bag bag2 elem3 custom-comparator)]

    (is (= 1 (count-nodes bag1)))
    (is (= 2 (count-nodes bag2)))
    (is (= 3 (count-nodes bag3)))
    (is (= 1 (find-count bag3 elem2 custom-comparator)))
    (is (= 2 (find-count (add-to-bag bag3 elem3 custom-comparator) elem3 custom-comparator)))))


(def neutral-element-prop
  (prop/for-all [x (gen/vector (gen/choose 1 10))]
                (let [bag (reduce add-to-bag empty-bag x)]
                  (= bag (combine-bags bag empty-bag)))))

;; (deftest test-neutral-element
;;   (quick-check 100 neutral-element-prop))
(deftest test-neutral-element
  (is (:pass? (quick-check 100 neutral-element-prop))
      "The neutral element does not change the structure of the bag"))

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
  (is (:pass? (quick-check 100 associative-combine-prop))
      "The associativity of the association must be respected"))

(def filter-idempotent-prop
  (prop/for-all [x (gen/vector (gen/choose 1 10))]
                (let [pred #(> % 5)
                      bag (reduce add-to-bag empty-bag x)
                      filtered-once (filter-bag bag pred)
                      filtered-twice (filter-bag filtered-once pred)]
                  (= filtered-once filtered-twice))))


(deftest test-filter-idempotent
  (is (:pass? (quick-check 100 filter-idempotent-prop))
      "Repeated filtering should not change the result"))

(def duplicates-prop
  (prop/for-all [x (gen/vector (gen/choose 1 10))]
                (let [bag (reduce add-to-bag empty-bag x)]
                  (every? (fn [el] (= (find-count bag el) (count (filter #(= % el) x)))) x))))

(deftest test-duplicates
  (is (:pass? (quick-check 100 duplicates-prop))
      "The multiset must correctly account for duplicates"))


(def combine-size-prop
  (prop/for-all [x (gen/vector (gen/choose 1 10))
                 y (gen/vector (gen/choose 1 10))]
                (let [bag1 (reduce add-to-bag empty-bag x)
                      bag2 (reduce add-to-bag empty-bag y)
                      combined (combine-bags bag1 bag2)]
                  (= (count-nodes combined)
                     (+ (count-nodes bag1) (count-nodes bag2))))))

(deftest test-combine-size
  (is (:pass? (quick-check 100 combine-size-prop))
      "The size of the combined multiset must be equal to the sum of the sizes of the original multisets"))


(def filter-size-prop
  (prop/for-all [x (gen/vector (gen/choose 1 10))]
                (let [pred #(> % 5)
                      bag (reduce add-to-bag empty-bag x)
                      filtered (filter-bag bag pred)]
                  (<= (count-nodes filtered) (count-nodes bag)))))

(deftest test-filter-size
  (is (:pass? (quick-check 100 filter-size-prop))
      "The number of items after filtering should decrease or remain the same"))
