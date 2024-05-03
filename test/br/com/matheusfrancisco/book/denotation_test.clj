(ns br.com.matheusfrancisco.book.denotation-test
  (:require
   [br.com.matheusfrancisco.book.denotations :refer [->Bool ->Numeric exec
                                                     ->Add
                                                     ->LessThan
                                                     ->Assign
                                                     ->While
                                                     ->If
                                                     ->Sequence
                                                     ->Multiply
                                                     ->Variable
                                                     invoke to->clj]]
   [clojure.test :refer [deftest is testing]]))

(deftest denotation

  (testing "Numeric =1"
    (is (= 1 (exec (->Numeric 1) {}))))

  (testing "Bool"
    (is (= true
           (-> (->Bool true) to->clj eval invoke))))

  (testing "Variable"
    (is (= 1 (-> (->Variable :x) (exec {:x 1}))))
    (is (= 1 (-> (->Variable :x) to->clj eval (invoke {:x 1})))))

  (testing "Add"
    (is (= 11
           (-> (->Add (->Numeric 1) (->Variable :y))
               (exec {:y 10}))))

    (is (= 3
           (-> (->Add (->Numeric 1) (->Numeric 2))
               to->clj
               eval
               invoke))))

  (testing "If"

    (is (= {:x 1 :y 11}
           (-> (->If (->LessThan (->Numeric 1) (->Numeric 4))
                     (->Assign :x (->Numeric 1))
                     (->Assign :x (->Numeric 2)))
               (exec {:y 11})))))

  (testing "While"
    (is (= {:x 20}
           (-> (->While
                (->LessThan (->Variable :x) (->Numeric 5))
                (->Assign :x (->Multiply (->Variable :x) (->Numeric 10))))
               (exec {:x 2}))))

    (is (= {:x 5}
           (-> (->While
                (->LessThan (->Variable :x) (->Numeric 5))
                (->Assign :x (->Add (->Variable :x) (->Numeric 1))))
               (exec {:x 1})))))
  (testing "Sequence"
    (is (= {:x 1 :y 3}
           (-> (->Sequence
                (->Assign :x (->Numeric 1))
                (->Assign :y (->Add (->Variable :x) (->Numeric 2))))
               (exec {})))))
;
  )
