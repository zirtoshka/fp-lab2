(ns laupok2.core-test
  (:require [clojure.test :refer :all]
            ;; [clojure.test.check :refer [quick-check]]
            ;; [clojure.test.check.properties :as prop]
            [laupok2.core :refer :all]))


(deftest add-remove-test
  (let [bag (-> empty-bag (add-node 5) (add-node 3) (add-node 5))]
    (is (= 3 (:count (add-node bag 5))))               ; Проверяем корректное добавление
    (is (= 1 (:count (remove-node bag 5))))          ; Проверяем корректное удаление
    (is (nil? (remove-node (remove-node bag 5) 5)))
    )
  )  ; Проверка полного удаления элемента

;; (deftest filter-map-test
;;   (let [bag (-> empty-bag (add-to-bag 5) (add-to-bag 3) (add-to-bag 7))
;;         filtered-bag (filter-bag bag #(> % 4))
;;         mapped-bag (map-bag bag #(* % 2))]
;;     (is (= 2 (:value (first (vals filtered-bag)))))     ; Проверка фильтрации
;;     (is (= 6 (:value (first (vals mapped-bag)))))))      ; Проверка map

;; (deftest fold-test
;;   (let [bag (-> empty-bag (add-to-bag 1) (add-to-bag 2) (add-to-bag 3))]
;;     (is (= 6 (fold-left-bag bag + 0)))                 ; Сумма значений (fold-left)
;;     (is (= 6 (fold-right-bag bag + 0)))))              ; Сумма значений (fold-right)

;; (deftest combine-test
;;   (let [bag1 (-> empty-bag (add-to-bag 1) (add-to-bag 2))
;;         bag2 (-> empty-bag (add-to-bag 3) (add-to-bag 4))]
;;     (is (= 4 (count (vals (combine-bags bag1 bag2))))))) ; Объединение двух bag





;; (def neutral-element-prop
;;   (prop/for-all [x (gen/vector (gen/choose 1 10))]
;;                 (let [bag (reduce add-to-bag empty-bag x)]
;;                   (= bag (combine-bags bag empty-bag)))))

;; (deftest test-neutral-element
;;   (quick-check 100 neutral-element-prop))



;; (def associative-combine-prop
;;   (prop/for-all [x (gen/vector (gen/choose 1 10))
;;                  y (gen/vector (gen/choose 1 10))
;;                  z (gen/vector (gen/choose 1 10))]
;;                 (let [bag1 (reduce add-to-bag empty-bag x)
;;                       bag2 (reduce add-to-bag empty-bag y)
;;                       bag3 (reduce add-to-bag empty-bag z)]
;;                   (= (combine-bags bag1 (combine-bags bag2 bag3))
;;                      (combine-bags (combine-bags bag1 bag2) bag3)))))

;; (deftest test-associativity
;;   (quick-check 100 associative-combine-prop))


;; (def filter-idempotent-prop
;;   (prop/for-all [x (gen/vector (gen/choose 1 10))
;;                  pred #(> % 5)]
;;                 (let [bag (reduce add-to-bag empty-bag x)
;;                       filtered-once (filter-bag bag pred)
;;                       filtered-twice (filter-bag filtered-once pred)]
;;                   (= filtered-once filtered-twice))))

;; (deftest test-filter-idempotent
;;   (quick-check 100 filter-idempotent-prop))

