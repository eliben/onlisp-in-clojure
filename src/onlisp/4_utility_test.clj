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

(deftest test-longer?
  (is (longer? '(a b c) '(x y)))
  (is (not (longer? '(a b c) '(x y z))))
  (is (not (longer? '(a b) '(x y z))))
  (is (longer? '(a) '()))
  (is (not (longer? '(x) '(y))))
  (is (not (longer? '() '())))
)

(deftest test-group
  (is (= (group '(a b c d) 2) '((a b) (c d))))
  (is (= (group '(a b c) 2) '((a b) (c))))
  (is (= (group '(a b c) 20) '((a b c))))
)

(deftest test-before?
  (is (before? 'a 'b '(x y a b c)))
  (is (before? 'a 'b '(a t b)))
  (is (before? 'a 'b '(a t c)))

  (is (not (before? 'a 'b '(t c))))
  (is (not (before? 'a 'b '(b t c))))
  (is (not (before? 'a 'b '(b a t c))))
  (is (not (before? 'a 'b '(b t c a))))
)

(run-tests)
