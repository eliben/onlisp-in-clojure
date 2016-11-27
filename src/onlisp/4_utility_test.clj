(ns onlisp.4-utility-test
  (:use clojure.test)
  (:use onlisp.4-utility))

(def mytowns '(Frobmund Nepur Barcity Bazburg))

(deftest test-find2
  (is (= ['Barcity '(bshop1)] (find2 bookshops mytowns))))

(deftest test-small-list-utils
  (let [lst1 '(foo bar kkk)]
    (is (= 'kkk (last1 lst1)))
    (is (nil? (last1 '())))
))

(run-tests)
