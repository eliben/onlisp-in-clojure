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
