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
