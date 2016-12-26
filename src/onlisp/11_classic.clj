(ns onlisp.11-classic
  (:use onlisp.macro-utils))

(defmacro our-let
  [binds & body]
  (let [vars (mapv (fn [b] (if (list? b) (first b) b)) binds)
        vals (mapv (fn [b] (if (list? b) (second b) nil)) binds)]
    `((fn ~vars
        ~@body)
      ~@vals)))

(mac
(our-let ((x 1) (y 2))
         (+ x y))
)

(defmacro with-gensyms
  [syms & body]
  `(let ~(vec (mapcat (fn [s] `(~s (gensym))) syms))
     ~@body))

(mac
(with-gensyms (gob x0 y1)
  (list gob x0 y1))
)

(defmacro nif
  [expr pos zero neg]
  (let [g (gensym)]
    `(let [~g ~expr]
       (cond
         (> ~g 0) ~pos
         (= ~g 0) ~zero
         :else ~neg))))

(map #(nif % 'p 'z 'n) '(1 -1 0 -2 7))

;;; Generates efficient code to check whether obj is one of the rest of the
;;; arguments.
(defmacro in
  [obj & choices]
  (let [insym (gensym)]
    `(let [~insym ~obj]
       (or ~@(map (fn [c] `(= ~insym ~c)) choices)))))

(mac
(in 'k 'a 'b 'c 'k 'd)
)

;;; Helper for 'in' that quotes arguments
(defmacro inq
  [obj & choices]
  `(in ~obj ~@(map (fn [c] `'~c) choices)))

(mac
(inq 'k a b c d k d)
)

(defn pairs
  [lst]
  (mapv #(take 2 (nthrest lst %)) (range 0 (dec (count lst)))))

;;; Equivalent of do-tuples/o on page 156. The name is Clojure-ized since / is
;;; for namespace separation.
(defmacro do-tuples-o
  [parms source & body]
  (if parms
    (let [src (gensym)]
      `(let [~src ~source]
         (doseq 
           [~parms (pairs ~src)]
           ~@body)))))

(mac
(do-tuples-o [x y] '(1 2 30 4 5) (prn (+ x y)))
)
