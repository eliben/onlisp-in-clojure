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
