(ns br.com.matheusfrancisco.book.dfa-test
  (:require
   [br.com.matheusfrancisco.book.dfa :refer [->Dfa ->DFARulebook ->FARule
                                             accepting? next-state]]
   [clojure.test :refer [deftest is]]))

(deftest test-dfa
  (let [r (->DFARulebook
           [(->FARule 1 'a' 2) (->FARule 1 'b' 1)
            (->FARule 2 'a' 2) (->FARule 2 'b' 3)
            (->FARule 3 'a' 3) (->FARule 3 'b' 3)])

        dfa (->Dfa 1 [1 3] r)
        dfa2 (->Dfa 1 [3] r)]

    (is (= (next-state r 1 'a') 2))
    (is (= (next-state r 1 'b') 1))
    (is (= (next-state r 2 'b') 3))

    (is (= (accepting? dfa) true))
    (is (= (accepting? dfa2) false))))
