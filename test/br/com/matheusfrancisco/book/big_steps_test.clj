(ns br.com.matheusfrancisco.book.big-steps-test
  (:require [br.com.matheusfrancisco.book.big-step :refer [->Add ->Assign ->LessThan
                                                           ->Numeric ->Variable ->While
                                                           ->If ->Sequences ->Bool ->Multiply
                                                           evaluate]]
            [clojure.test :refer [deftest is testing]]))

(deftest big-steps
  (testing ""
    (is (= 20 (:value (evaluate (->Multiply (->Numeric 10) (->Numeric 2)) {})))))

  (testing "Add"
    (is (= 3 (:value (evaluate (->Add (->Numeric 1) (->Numeric 2)) {})))))

  (testing "Sequences"
    (is (= {:x #br.com.matheusfrancisco.book.big_step.Numeric{:value 2}
            :y #br.com.matheusfrancisco.book.big_step.Numeric{:value 5}}
           (evaluate (->Sequences
                      (->Assign :x (->Add (->Numeric 1) (->Numeric 1)))
                      (->Assign :y (->Add (->Variable :x) (->Numeric 3)))) {}))))
  (testing "if"
    (is (= 1 (:value (:x (evaluate (->If (->Bool true)
                                         (->Assign :x (->Numeric 1))
                                         (->Assign :y (->Numeric 2))) {}))))))
  (testing "while"
    (is (= 6
           (:value (:x (evaluate (->While
                                  (->LessThan (->Variable :x) (->Numeric 6))
                                  (->Assign :x (->Add (->Variable :x) (->Numeric 1))))
                                 {:x (->Numeric 0)}))))))
  (testing "LessThan? 3 < 4"
    (is (= true (:value (evaluate (->LessThan (->Numeric 3) (->Numeric 4)) {}))))))
