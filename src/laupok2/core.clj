(ns laupok2.core
  (:gen-class))

(defprotocol Bag
  "for multiset (bag)"
  (add-to-bag [bag element] "add element")
  (remove-from-bag [bag element] "delete element")
  (filter-bag [bag pred] "filter bag by predicate")
  (map-bag [bag f] "mapping a function to elements")
  (fold-left-bag [bag f init] "left fold")
  (fold-right-bag [bag f init] "right fold")
  (combine-bags [bag1 bag2])
  (compare-bags [bag1 bag2]))

(defrecord TreeNode [value count left right])

(def empty-bag nil)

(extend-type nil
  Bag
  (add-to-bag [_ element] (->TreeNode element 1 nil nil))
  (remove-from-bag [_ _] nil)
  (filter-bag [_ _] nil)
  (map-bag [_ _] nil)
  (fold-left-bag [_ _ acc] acc)
  (fold-right-bag [_ _ acc] acc)
  (combine-bags [_ bag2] bag2))

(defn add-node
  [node element]
  (cond
    (nil? node) (->TreeNode element 1 nil nil)
    (= element (:value node)) (update node :count inc)
    (< element (:value node)) (assoc node :left (add-node (:left node) element))
    :else (assoc node :right (add-node (:right node) element))))

(defn merge-trees
  [left right]
  (if (nil? left)
    right
    (assoc left :right (merge-trees (:right left) right))))

(defn remove-node
  [node element]
  (cond
    (nil? node) nil
    (= element (:value node))
    (if (> (:count node) 1)
      (update node :count dec)
      (merge-trees (:left node) (:right node)))
    (< element (:value node)) (assoc node :left (remove-node (:left node) element))
    :else (assoc node :right (remove-node (:right node) element))))

(defn filter-tree
  [node pred]
  (when node
    (let [left (filter-tree (:left node) pred)
          right (filter-tree (:right node) pred)]
      (if (pred (:value node))
        (->TreeNode (:value node) (:count node) left right)
        (merge-trees left right)))))

(defn map-tree
  [node f]
  (when node
    (let [new-value (f (:value node))
          left (map-tree (:left node) f)
          right (map-tree (:right node) f)]
      (->TreeNode new-value (:count node) left right))))

(defn fold-left-tree
  [node f acc]
  (if node
    (let [acc-left (fold-left-tree (:left node) f acc)
          node-acc (reduce (fn [a _] (f a (:value node))) acc-left (range (:count node)))]
      (fold-left-tree (:right node) f node-acc))
    acc))

(defn fold-right-tree
  [node f acc]
  (if node
    (let [node-acc (reduce (fn [a _] (f (:value node) a)) acc (range (:count node)))
          acc-right (fold-right-tree (:right node) f node-acc)]
      (fold-right-tree (:left node) f acc-right))
    acc))

(defn combine-bags
  [bag1 bag2]
  (fold-left-tree bag2 add-node bag1))

(defn compare-bags
  [bag1 bag2]
  (cond
    (and (nil? bag1) (nil? bag2)) true
    (or (nil? bag1) (nil? bag2)) false
    (and (= (:value bag1) (:value bag2))
         (= (:count bag1) (:count bag2))
         (compare-bags (:left bag1) (:left bag2))
         (compare-bags (:right bag1) (:right bag2))) true
    :else false))

(extend-type TreeNode
  Bag
  (add-to-bag [bag element] (add-node bag element))
  (remove-from-bag [bag element] (remove-node bag element))
  (filter-bag [bag pred] (filter-tree bag pred))
  (map-bag [bag f] (map-tree bag f))
  (combine-bags [bag1 bag2] (combine-bags bag1 bag2)))


(defn find-count [node value]
  (cond
    (nil? node) nil  ; Если узел пустой, возвращаем nil
    (= (:value node) value) (:count node)  ; Если значение узла совпадает с искомым, возвращаем счетчик
    (< (hash value) (hash (:value node))) (find-count (:left node) value)  ; Ищем в левом поддереве
    :else (find-count (:right node) value)))  ; Ищем в правом поддереве

;; Testing the implementation
(let [bagi (-> empty-bag
               (add-node 5)
               (add-node 3)
               (add-node 7)
               (add-node 5))]
  ;; (println (filter-tree bagi #(> % 4)))
  ;; (println(    :count (add-node bagi 5)               ; Проверяем корректное добавление
  ;;          ))
  (println( find-count (remove-node bagi 3) 3 ))
  ;; (println (map-tree bagi #(* % 2)))
  ;; (println (fold-left-tree bagi + 0))
  ;; (println (fold-right-tree bagi + 0))
  (println (combine-bags bagi (add-node empty-bag 10))))

;; (let [bag1 (-> empty-bag (add-to-bag 1) (add-to-bag 2))
;;       bag2 (-> empty-bag (add-to-bag 3) (add-to-bag 4))
;;       bag3 (-> empty-bag (add-to-bag 5) (add-to-bag 6))]
;;   (println (combine-bags bag1 (combine-bags bag2 bag3)))
;;   (println (combine-bags (combine-bags bag1 bag2) bag3)))
