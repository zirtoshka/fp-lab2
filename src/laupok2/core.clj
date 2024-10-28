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
  (add [bag element] "add elemet")
  ;; (remove-from-bag [bag element] "delete elemet")
  ;; (filter-bag [bag pred] "filter bag by predicat")
  ;; (map-bag [bag f] "mapping a function to elemets")
  ;; (fold-left-bag [bag f init] "left fold")
  ;; (fold-right-bag [bag f init] "right fold")
  )

(defrecord TreeNode [value count left right])

(def empty-bag nil)

(extend-type nil
  Bag 
  (add [_ element] (->TreeNode element 1 nil nil)))


;; добавление и удаление элементов;

(defn add-node [node element]
  (cond
    (nil? node) (->TreeNode element 1 nil nil)
    (= element (:value node)) (update node :count inc)
    (< (hash element) (hash(:value node))) (assoc node :left (add-node (:left node) element))
    :else (assoc node :right (add-node (:right node) element))))


(extend-type TreeNode
  Bag
  (add [bag element] (add-node bag element)))

(let [bagi (-> empty-bag
              (add "1")
              (add "2")
              (add "1"))]
  (println bagi))

(< (hash {:kok 1}) (hash {:kok 2}))

