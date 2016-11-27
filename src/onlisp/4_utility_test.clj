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
    
    (is (single? '(sdf)))
    (is (not (single? '())))
    (is (not (single? nil)))
    (is (not (single? lst1)))

    (is (= '(kkk) (conc1 '() 'kkk)))
    (is (= '(foo bar kkk dwa) (conc1 lst1 'dwa)))

    (is (= '(kwa) (mklist '(kwa))))
    (is (= '(kwa) (mklist 'kwa)))
    (is (= '(joe moe) (mklist (list 'joe 'moe))))
))

(run-tests)
