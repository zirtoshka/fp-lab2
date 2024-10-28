(ns laupok2.core
  (:gen-class))


;; Функции:

;; добавление и удаление элементов;
;; фильтрация;
;; отображение (map);
;; свертки (левая и правая);
;; структура должна быть моноидом.


(defprotocol Bag
  "for multiset (bag)"
  (add-to-bag [bag element] "add elemet")
  (remove-from-bag [bag element] "delete elemet")
  (filter-bag [bag pred] "filter bag by predicat")
  (map-bag [bag f] "mapping a function to elemets")
  (fold-left-bag [bag f init] "left fold")
  (fold-right-bag [bag f init] "right fold")
  (combine-bags [bag1 bag2]))

(defrecord TreeNode [value count left right])

(def empty-bag nil)

(extend-type nil
  Bag
  (add-to-bag [_ element] (->TreeNode element 1 nil nil))
  (remove-from-bag [_ _] nil)
  (filter [_ _] nil)
  (map [_ _] nil)
  (fold-left-bag [_ _ acc] acc)
  (fold-right-bag [_ _ acc] acc)
  (combine-bags [_ bag2] bag2))


;; добавление элементов;

(defn add-node
  [node element]
  (cond
    (nil? node) (->TreeNode element 1 nil nil)
    (= element (:value node)) (update node :count inc)
    (< (hash element) (hash (:value node))) (assoc node :left (add-node (:left node) element))
    :else (assoc node :right (add-node (:right node) element))))

(< (hash {:kok 1}) (hash {:kok 2}))

;; удаление элементов;

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
    (< (hash element) (hash (:value node))) (assoc node :left (remove-node (:left node) element))
    :else (assoc node :right (remove-node (:right node) element))))




;; фильтрация;
(defn filter-tree
  [node pred]
  (when node
    (let [left (filter-tree (:left node) pred)
          right (filter-tree (:right node) pred)]
      (if (pred (:value node))
        (->TreeNode (:value node) (:count node) left right)
        (merge-trees left right)))))

;; отображение (map);
(defn map-tree
  [node f]
  (when node
    (let [new-value (f (:value node))
          left (map-tree (:left node) f)
          right (map-tree (:right node) f)]
      (->TreeNode new-value (:count node) left right))))

;; свертки (левая и правая);
(defn fold-left-tree
  [node f acc]
  (if node
    (let [acc-left (fold-left-tree (:left node) f acc)
          acc-node (reduce (fn [a _] (f a (:value node))) acc-left
                           (repeat (:count node) nil))
          acc-right (fold-left-tree (:right node) f acc-node)]
      acc-right)
    acc))

(defn fold-right-tree
  [node f acc]
  (if node
    (let [acc-right (fold-right-tree (:right node) f acc)
          acc-node (reduce (fn [a _] (f (:value node) a)) acc-right
                           (repeat (:count node) nil))
          acc-left (fold-right-tree (:left node) f acc-node)]
      acc-left)
    acc))


;; структура должна быть моноидом.
(defn combine-bags
  [bag1 bag2]
  (fold-left-tree bag2 add-node bag1))


(extend-type TreeNode
  Bag
  (add-to-bag [bag element] (add-node bag element))
  (remove-from-bag [bag element] (remove-node bag element))
  (filter-bag [bag element] (filter-tree bag element))
  (map-bag [bag element] (map-tree bag element))
  (combine-bags [bag1 bag2] (combine-bags bag1 bag2)))


(let [bagi (-> empty-bag
               (add-node 5)
               (add-node 3)
               (add-node 7)
               (add-node 5))]
  (println (filter-tree bagi #(> % 4)))
  (println (map-tree bagi #(* % 2)))
  (println (fold-left-tree bagi + 0))        
  (println (fold-right-tree bagi + 0))
  (println (combine-bags bagi (add-node empty-bag 10)))
  )


(let [bag1 (-> empty-bag (add-to-bag 1) (add-to-bag 2))
      bag2 (-> empty-bag (add-to-bag 3) (add-to-bag 4))
      bag3 (-> empty-bag (add-to-bag 5) (add-to-bag 6))]
  (println (combine-bags bag1 (combine-bags bag2 bag3)))
  (println (combine-bags (combine-bags bag1 bag2) bag3)))