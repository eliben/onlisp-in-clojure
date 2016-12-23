(ns onlisp.10-pitfalls
  (:use onlisp.macro-utils))

(defn or-expand
  "Recursive expansion for ora"
  [args]
  (if (empty? args)
    nil
    (let [sym (gensym)]
      `(let [~sym ~(first args)]
         (if ~sym
           ~sym
           ~(or-expand (rest args)))))))

(defmacro ora
  [& args]
  (or-expand args))

(defmacro orb
  [& args]
  (if (empty? args)
    nil
    (let [sym (gensym)]
      `(let [~sym ~(first args)]
         (if ~sym
           ~sym
           (orb ~@(rest args)))))))
  
(mac
(ora false nil 3)
)

(mac
(orb false nil 3)
)
