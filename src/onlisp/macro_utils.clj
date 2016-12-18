(ns onlisp.macro-utils)

(defmacro mac
  "Pretty-print a 1-level macro expansion of 'expr'. 'expr' should be a regular
  Clojure expression (there's no need to quote it)."
  [expr]
  `(clojure.pprint/pprint (macroexpand-1 '~expr)))
