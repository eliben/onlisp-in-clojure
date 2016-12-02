(ns onlisp.4-utility)

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

(defn find2
  "find2 from the book. Given a function and a list, returns the first entry in
  the list where the function applied to it results in a non-nil value. Returns
  a vector of [list-value (func list-value)]."
  [func lst]
  (if (empty? lst)
    nil
    (let [val (func (first lst))]
      (if val
        [(first lst) val]
        (find2 func (rest lst))))))

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

(defn last1
  "Last element in a list."
  [lst]
  (last lst))

(defn single?
  "Is lst a list of one element?."
  [lst]
  (and (seq lst)
       (not (seq (rest lst)))))

(defn conc1
  "Concatenates a single element to list."
  [lst obj]
  (concat lst (list obj)))

;;; Not defining conc1 here, since we don't do destructive list modifications in
;;; Clojure.

(defn mklist
  "Makes a list out of obj, unless it's already a list, and return it."
  [obj]
  (if (list? obj)
    obj
    (list obj)))

(defn longer?
  "Returns true iff xlst is longer than ylst"
  [xlst ylst]
  (letfn [(compare-lists [x y]
            (and (seq x)
                 (or (empty? y)
                     (compare-lists (rest x) (rest y)))))]
    (if (and (list? xlst) (list? ylst))
      (compare-lists xlst ylst)
      (> (count xlst) (count ylst)))))

(defn group
  "Split source into groups of n lists; the last group may be incomplete."
  [source n]
  ;; Use nil-padding in partition to prevent it from dropping the last group if
  ;; isn't complete.
  ;; An alternative here would be to use partition-all.
  (partition n n nil source))

(defn before?
  "Does x appear in lst before y? Note that it y doesn't appear in the list at
  all and x does, true is returned."
  ([x y lst]
   (before? x y lst =))

  ([x y lst testfn]
   (and (seq lst)
        (let [head (first lst)]
          (cond
            (testfn y head) false
            (testfn x head) true
            :else (before? x y (rest lst) testfn))))))
