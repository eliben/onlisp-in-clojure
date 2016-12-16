(ns onlisp.5-returning-functions-test
  (:use clojure.test)
  (:use onlisp.5-returning-functions))

(deftest test-compose
  (is (= '(4 3) ((compose #(cons 4 %) list inc) 2)))
  (is (= '(8) ((compose #(map inc %) filter) odd? '(2 7 4))))
)

(deftest test-mycomplement
  (let [not-odd? (my-complement odd?)]
    (is (not (not-odd? 5)))
    (is (not-odd? 6))
))

(deftest test-our-length-every
  (let [intlst1 '(5 7 9)
        intlst2 '(5 7 6 9)
        emptylst '()]
    (is (= 3 (our-length intlst1)))
    (is (our-every odd? intlst1))

    (is (= 4 (our-length intlst2)))
    (is (not (our-every odd? intlst2)))

    (is (= 0 (our-length emptylst)))
    (is (our-every odd? emptylst))
    (is (our-every even? emptylst))
))

(run-tests)
