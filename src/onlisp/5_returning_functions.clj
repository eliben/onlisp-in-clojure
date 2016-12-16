(ns onlisp.5-returning-functions)

(defn compose
  [& fns]
  (if (seq fns)
    (let [fnsrev (reverse fns)
          fn1 (first fnsrev)
          fns (rest fnsrev)]
      (fn [& args]
        (reduce
          (fn [result nextfn] (nextfn result))
          (apply fn1 args)
          fns)))
    (fn [& args] args)))

((compose #(cons 4 %1) list inc) 2)

((compose #(map inc %) filter) odd? '(2 7 4))

(defn my-complement
  [pred]
  (compose not pred))

((my-complement odd?) 5)
((my-complement odd?) 6)
