(ns onlisp.7-macros
  (:use onlisp.macro-utils))

(defmacro our-when
  [tst & body]
  `(if ~tst
     (do ~@body)))

(def foo 3)
(mac
 (our-when (> foo 4)
           (prn foo)
           (prn (inc foo))))

;;; Clojure already has 'while'; this recreates it with a loop. Note that 'when'
;;; is also a macro, so a full macro expansion will expand that as well.
(defmacro our-while
  [tst & body]
  `(loop []
     (when ~tst
       ~@body
       (recur))))

(mac
 (our-while hungry
            (stare-intently)
            (meow)))

;;; Similar to Clojure's when-let
(defmacro when-bind
  [[var expr] & body]
  `(let [~var ~expr]
     (when ~var
       ~@body)))

(mac
 (when-bind (inp (get-user-input))
            (process input)))

(def alst '())
(when-bind (s (seq alst))
           (rest s))

;;; Implementing CL's do macro
;;;
;;; (our-do
;;;      ((w 3)
;;;       (x 1 (inc x))
;;;       (y 2 (inc y))
;;;       (z))
;;;     ((> x 10) (prn z) y)
;;;  (prn x)
;;;  (prn y))
;;;
;;; Should expand to:
;;;
;;; (loop [w 3
;;;        x 1
;;;        y 2
;;;        z nil]
;;;   (if (> x 10)
;;;     (do (prn z) y)
;;;     (do (prn x) (prn y) (recur w (inc x) (inc y) z))))

(defn make-initforms
  [bindforms]
  (mapcat
   (fn [b]
     (if (seq b)
       [(first b) (second b)]
       [b nil]))
   bindforms))

(defn make-stepforms
  [bindforms]
  (map
   (fn [b]
     (if (and (seq b) (> (count b) 2))
       (nth b 2)
       (first b)))
   bindforms))

(make-initforms '((w 3) (x 1 (inc x)) (y 2 (inc y)) (z)))
(make-stepforms '((w 3) (x 1 (inc x)) (y 2 (inc y)) (z)))

(defmacro our-do
  [bindforms [test & result] & body]
  `(loop ~(vec (make-initforms bindforms))
     (if ~test
       (do ~@result)
       (do
         ~@body
         (recur ~@(make-stepforms bindforms))))))

;(mac
 ;(our-do
      ;((w 3)
       ;(x 1 (inc x))
       ;(y 2 (inc y))
       ;(z))
     ;((> x 10) (prn z) y)
  ;(prn x)
  ;(prn y))
;)

;(loop [w 3
      ;x 1
      ;y 2
      ;z nil]
 ;(if (> x 10)
   ;(do (prn z) y)
   ;(do
     ;(prn x)
     ;(prn y)
     ;(recur w (inc x) (inc y) z))))

(defmacro our-for
  [[var start stop] & body]
  (let [gstop (gensym 'stop)]
    `(our-do ((~var ~start (inc ~var))
              (~gstop ~stop))
             ((> ~var ~gstop))
             ~@body)))

;(our-for
  ;(i 1 10)
  ;(prn i))

