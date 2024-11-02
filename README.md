# Лабораторная работа №2

## Титульный лист

**Студент**: Зайцева Ирина Сергеевна

**Группа**: P3309

**ИСУ**: 367222


## Требования к разработанному ПО

1. **Функциональные требования:**
   - Реализовать multiset binary tree.
   - Поддержка следующих функций:
     - Добавление и удаление элементов.
     - Фильтрация.
     - Применение функции к каждому элементу (отображение).
     - Свертка элементов (левая и правая).
     - Поддержка структуры как моноида.
  
2. **Ненормативные требования:**
   - Структуры данных должны быть неизменяемыми.
   - Библиотека должна быть протестирована с использованием юнит-тестирования и property-based тестирования (минимум 3 свойства, включая свойства моноида).
   - Структура должна быть полиморфной.
   - Использовать идиоматичный стиль программирования для языка.

---

## Ключевые элементы реализации

```clojure
(defprotocol Bag
  (add-to-bag [bag element] [bag element cmp])
  (remove-from-bag [bag element] [bag element cmp])
  (filter-bag [bag pred])
  (map-bag [bag f])
  (fold-left-bag [bag f init])
  (fold-right-bag [bag f init])
  (combine-bags [bag1 bag2])
  (compare-bags [bag1 bag2] [bag element cmp])
  (count-nodes [node])
  (find-count [bag element] [bag element cmp]))

(defrecord TreeNode [value count left right])

(def empty-bag nil)

(extend-type nil
  Bag
  ;; Определение методов для пустого дерева
  ...)

(extend-type TreeNode
  Bag
  ;; Определение методов для узлов дерева
  ...)
```

### Основные методы
- **add-to-bag**: Добавляет элемент в бинарное дерево, увеличивая счётчик, если элемент уже существует.
```clojure
(add-to-bag
    ([bag element cmp]
     (let [cmp-res (cmp element (:value bag))]
       (cond
         (nil? bag) (->TreeNode element 1 nil nil)
         (= 0 cmp-res)   (update bag :count inc)
         (neg? cmp-res)  (assoc bag :left (add-to-bag (:left bag) element cmp))
         :else       (assoc bag :right (add-to-bag (:right bag) element cmp)))))
    ([bag element]
     (add-to-bag bag element compare)))
```
- **remove-from-bag**: Удаляет элемент из дерева, корректируя счётчик или объединяя поддеревья.
```clojure
(remove-from-bag
    ([bag element cmp]
     (let [cmp-res (cmp element (:value bag))]
       (cond
         (nil? bag) nil
         (= 0 cmp-res) ;; элемент найден
         (if (> (:count bag) 1)
           (update bag :count dec)
           (merge-trees (:left bag) (:right bag)))
         (neg? cmp-res) ;; element меньше, чем значение в узле
         (assoc bag :left (remove-from-bag (:left bag) element cmp))
         :else ;; element больше, чем значение в узле
         (assoc bag :right (remove-from-bag (:right bag) element cmp)))))
    ([bag element]
     (remove-from-bag bag element compare)))
```
- **filter-bag**: Фильтрует элементы дерева по заданному предикату.
```clojure
  (filter-bag [bag pred]
    (when bag
      (let [left (filter-bag (:left bag) pred)
            right (filter-bag (:right bag) pred)]
        (if (pred (:value bag))
          (->TreeNode (:value bag) (:count bag) left right)
          (merge-trees left right)))))
```
- **map-bag**: Применяет функцию к каждому элементу дерева.
```clojure
(map-bag [bag f]
    (when bag
      (let [new-value (f (:value bag))
            left (map-bag (:left bag) f)
            right (map-bag (:right bag) f)]
        (->TreeNode new-value (:count bag) left right))))
```
- **fold-left-bag** и **fold-right-bag**: Реализуют свёртку элементов с учетом их порядка.
```clojure
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

```
- **combine-bags**: Объединяет два множества в одно.
```clojure
  (combine-bags [bag1 bag2] (fold-left-bag bag2 add-to-bag bag1))
```
---

## Тестирование

[Тесты](/test/laupok2/) проходят упешно, что отражается в CI.

---

## Выводы

В ходе выполнения лабораторной работы удалось освоить следующие концепции: создание пользовательских типов данных, применение полиморфизма. Использованные подходы, такие как создание протоколов и расширение типов, помогли реализовать multiset binary tree.