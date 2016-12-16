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

(defn lrec
  "Build a recursive-on-list function from primitives. rec is a function taking
  two parameters: the current first element in the list, and a function which
  can be called with no arguments to continue the recursion. base is an optional
  starting point."
  ([rec] (lrec rec '()))
  ([rec base]
   (letfn [(self [lst]
             (if (empty? lst)
               (if (fn? base)
                 (base)
                 base)
               (rec (first lst)
                    (fn [] (self (rest lst))))))]
     self)))
