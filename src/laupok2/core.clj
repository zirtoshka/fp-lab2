(ns laupok2.core
  (:gen-class))

(defprotocol Bag
  "for multiset (bag)"
  (add-to-bag [bag element] "add element") ;;[bag element cmp] add
  (remove-from-bag [bag element] "delete element")
  (filter-bag [bag pred] "filter bag by predicate")
  (map-bag [bag f] "mapping a function to elements")
  (fold-left-bag [bag f init] "left fold")
  (fold-right-bag [bag f init] "right fold")
  (combine-bags [bag1 bag2] "comb")
  (compare-bags [bag1 bag2] "comp")
  (count-nodes [node]))

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
  (combine-bags [_ bag2] bag2)
  (compare-bags [_ bag2] (nil? bag2))
  (count-nodes [_] 0))

(defn merge-trees
  [left right]
  (if (nil? left)
    right
    (assoc left :right (merge-trees (:right left) right))))

(extend-type TreeNode
  Bag
  (add-to-bag [bag element]
    (let [cmp-res (compare element (:value bag))]
      (cond
        (nil? bag) (->TreeNode element 1 nil nil)
        (= 0 cmp-res)   (update bag :count inc)
        (neg? cmp-res)  (assoc bag :left (add-to-bag (:left bag) element))
        :else       (assoc bag :right (add-to-bag (:right bag) element)))))

  (remove-from-bag [bag element]
    (let [cmp (compare element (:value bag))]
      (cond
        (nil? bag) nil
        (= 0 cmp) ;; элемент найден
        (if (> (:count bag) 1)
          (update bag :count dec)
          (merge-trees (:left bag) (:right bag)))
        (neg? cmp) ;; element меньше, чем значение в узле
        (assoc bag :left (remove-from-bag (:left bag) element))
        :else ;; element больше, чем значение в узле
        (assoc bag :right (remove-from-bag (:right bag) element)))))

  (filter-bag [bag pred]
    (when bag
      (let [left (filter-bag (:left bag) pred)
            right (filter-bag (:right bag) pred)]
        (if (pred (:value bag))
          (->TreeNode (:value bag) (:count bag) left right)
          (merge-trees left right)))))

  (map-bag [bag f]
    (when bag
      (let [new-value (f (:value bag))
            left (map-bag (:left bag) f)
            right (map-bag (:right bag) f)]
        (->TreeNode new-value (:count bag) left right))))

  (fold-left-bag [bag f init]
    (if bag
      (let [acc-left (fold-left-bag (:left bag) f init)
            node-acc (reduce (fn [a _] (f a (:value bag))) acc-left (range (:count bag)))]
        (fold-left-bag (:right bag) f node-acc))
      init))

  (fold-right-bag [bag f init]
    (if bag
      (let [node-acc (reduce (fn [a _] (f (:value bag) a)) init (range (:count bag)))
            acc-right (fold-right-bag (:right bag) f node-acc)]
        (fold-right-bag (:left bag) f acc-right))
      init))

  (combine-bags [bag1 bag2] (fold-left-bag bag2 add-to-bag bag1))

  (compare-bags [bag1 bag2]
    (cond
      (and (nil? bag1) (nil? bag2)) true
      (or (nil? bag1) (nil? bag2)) false
      (and (= 0 (compare (:value bag1) (:value bag2)))
           (= (:count bag1) (:count bag2))
           (compare-bags (:left bag1) (:left bag2))
           (compare-bags (:right bag1) (:right bag2))) true
      :else false))
  (count-nodes [node]
    (if node
      (+ (:count node)  ; Считаем текущий узел
         (count-nodes (:left node))  ; Считаем узлы в левом поддереве
         (count-nodes (:right node)))  ; Считаем узлы в правом поддереве
      0)))

(defn find-count [node value]
  (let [cmp (compare value (:value node))]
    (cond
      (nil? node) nil  ; пустой -  nil
      (= 0 cmp) (:count node)  ;  совпадают - счетчик
      (neg? cmp) (find-count (:left node) value)  ; меньше, ищем в левом поддереве
      :else (find-count (:right node) value))))  ;  ищем в правом поддереве

(def bagi (-> empty-bag (add-to-bag 3) (add-to-bag 5) (add-to-bag 7)))
(println (:value bagi))
(println  (:value (first (filter-bag bagi #(> % 4)))))
    ;; (is (= 2 (:value (first (vals filtered-bag)))))     

(def bag1 (-> empty-bag (add-to-bag 1) (add-to-bag 2)))
(def bag2 (-> empty-bag (add-to-bag 3) (add-to-bag 4)))
(println (count-nodes (combine-bags bag1 bag2)))

(println (:value bagi))

(println (map-bag bagi #(* % 2)))

;; (def bagi (-> empty-bag (add-to-bag "3")
;;               (add-to-bag "4")
;;               (remove-from-bag "2")))
;; (println bagi)

