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
  )

(defrecord TreeNode [value count left right])

(def empty-bag nil)

(extend-type nil
  Bag
  (add-to-bag [_ element] (->TreeNode element 1 nil nil))
  (remove-from-bag [_ _] nil)
  (filter [_ _] nil)
  (map [_ _] nil)
  (fold-left-bag [_ _ acc] acc)
  (fold-right-bag [_ _ acc] acc))


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
        (merge-trees left right))))
  )

;; отображение (map);
;; свертки (левая и правая);
;; структура должна быть моноидом.



(extend-type TreeNode
  Bag
  (add-to-bag [bag element] (add-node bag element))
  (remove-from-bag [bag element] (remove-node bag element))
  (filter-bag [bag element] (filter-tree bag element)))


(let [bagi (-> empty-bag
               (add-to-bag 5)
               (add-to-bag 3)
               (add-to-bag 5)
               (add-to-bag 7))
      filtered-bag (filter-tree bagi #(> % 4))] ; Оставляем только элементы больше 4
  (println filtered-bag))
