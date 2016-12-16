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

(defn my-complement
  [pred]
  (compose not pred))

(defn our-length
  [lst]
  (if (empty? lst)
    0
    (inc (our-length (rest lst)))))

(defn our-every
  [fun lst]
  (if (empty? lst)
    true
    (and (fun (first lst))
         (our-every fun (rest lst)))))
