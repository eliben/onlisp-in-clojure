(ns onlisp.4-utility)

(def towns '(Frobmund Nepur Barcity Bazburg))
(defn bookshops
  [town]
  (case town
    Frobmund nil
    Nepur nil
    Barcity '(bshop1)
    Bazburg '(zshop1 zshop2 zshop3)
    (assert false "bad town")))

(defn find-books
  [towns]
  (if (empty? towns)
    nil
    (let [shops (bookshops (first towns))]
      (if shops
        [(first towns) shops]
        (find-books (rest towns))))))

(find-books towns)

(defn find2
  "find2 from the book."
  [func lst]
  (if (empty? lst)
    nil
    (let [val (func (first lst))]
      (if val
        [(first lst) val]
        (find2 func (rest lst))))))

(find2 bookshops towns)

(defn find2-clj
  "A more idiomatic find2 with a loop."
  [func lst]
  (loop [l lst]
    (if (empty? l)
      nil
      (let [val (func (first l))]
        (if val
          [(first l) val]
          (recur (rest l)))))))
  
(find2-clj bookshops towns)
