(ns br.com.matheusfrancisco.book.core-test
  (:require
   [br.com.matheusfrancisco.book.core :refer [->Add
                                              ->Assign
                                              ->While
                                              ->DoNothing
                                              ->Sequence
                                              ->Bool
                                              ->LessThan
                                              ->If
                                              ->Multiply
                                              ->Numeric
                                              ->Variable
                                              -reduced machine-run]]
   [clojure.test :refer [deftest is testing]]))

(deftest test-small-step-semantics
  (testing "build a tree with the expression (1 * 2) + (3 * 4)"
    (is (= #br.com.matheusfrancisco.book.core.Add
            {:left #br.com.matheusfrancisco.book.core.Multiply
                    {:left #br.com.matheusfrancisco.book.core.Numeric{:value 1},
                     :right #br.com.matheusfrancisco.book.core.Numeric{:value 2}}
             :right #br.com.matheusfrancisco.book.core.Multiply
                     {:left #br.com.matheusfrancisco.book.core.Numeric{:value 3},
                      :right #br.com.matheusfrancisco.book.core.Numeric{:value 4}}}
           (->Add
            (->Multiply (->Numeric 1) (->Numeric 2))
            (->Multiply (->Numeric 3) (->Numeric 4))))))

  (testing "print the expression (1 * 2) + (3 * 4)"
    (is (= "((1 * 2) + (3 * 4))"
           (str (->Add
                 (->Multiply (->Numeric 1) (->Numeric 2))
                 (->Multiply (->Numeric 3) (->Numeric 4)))))))

  (testing "less than 3 < 4"
    (is (= "3 < 4"
           (str (->LessThan (->Numeric 3) (->Numeric 4))))))

  (testing "->Bool"
    (is (= "true"
           (str (->Bool true)))))

  (testing "Variable"
    (is (= "7"
           (str
            (-> (machine-run (->Assign :k (->Add
                                           (->Variable :x)
                                           (->Variable :y)))
                             {:x (->Numeric 3), :y (->Numeric 4)})
                second
                :k)))))

  (testing "If statement"
    (is (= "1"
           (-> (machine-run
                (->If (->Bool true)
                      (->Assign :x (->Numeric 1))
                      (->Assign :y (->Numeric 2))) {})
               second
               :x
               str))))

  (testing "If if(x) {y=1} else {do-nothing}"
    (is (= [#br.com.matheusfrancisco.book.core.DoNothing{}
            {:x #br.com.matheusfrancisco.book.core.Bool{:value false}}]
           (-> (machine-run
                (->If (->Variable :x)
                      (->Assign :y (->Numeric 1))
                      (->DoNothing))
                {:x (->Bool false)})))))

  (testing "if(x) {y=1} else {y=2}"
    (is (= "1"
           (-> (machine-run
                (->If (->Variable :x)
                      (->Assign :y (->Numeric 1))
                      (->Assign :y (->Numeric 2)))
                {:x (->Bool true)})
               second
               :y
               str))))

  (testing "Assing"
    (let [[_ env]
          (machine-run
           (->Assign :x (->Add (->Numeric 1) (->Numeric 2))) {})]
      (is (= "3" (-> env :x str)))))

  (testing "Sequence x = 1 +1;y= x+3"
    (let [result (-> (machine-run
                      (->Sequence
                       (->Assign :x (->Add (->Numeric 1) (->Numeric 1)))
                       (->Assign :y (->Add (->Variable :x) (->Numeric 3))))
                      {})
                     second)] (is (=
                                   "5"
                                   (-> result :y str)))))

  (testing "while (x < 5) {x = x+1}"
    (is (= "while (x < 5) { x = (x + 1) }"
           (-> (->While
                (->LessThan (->Variable :x) (->Numeric 5))
                (->Assign :x (->Add (->Variable :x) (->Numeric 1))))
               str))))

  (testing "while (x < 5) {x = x+1}"
    (is (= "5"
           (str (:x (second (machine-run
                             (->While
                              (->LessThan (->Variable :x) (->Numeric 5))
                              (->Assign :x (->Add (->Variable :x) (->Numeric 1))))
                             {:x (->Numeric 1)}))))))))

(deftest test-reduce
  (testing "reduce the expression (1 * 2) + (3 * 4) to 14"
    (is (= #br.com.matheusfrancisco.book.core.Numeric{:value 14}
           (-> (machine-run
                (->Assign :result (->Add
                                   (->Multiply (->Numeric 1) (->Numeric 2))
                                   (->Multiply (->Numeric 3) (->Numeric 4)))) {})
               second
               :result))))

  (testing "lessthan expression 5 < 2 + 2"
    (is (= "false"
           (str (-> (->LessThan
                     (->Numeric 5)
                     (->Add
                      (->Numeric 2)
                      (->Numeric 2)))
                    (-reduced {})
                    (-reduced {})))))

    (is (= #br.com.matheusfrancisco.book.core.Bool{:value false}
           (-> (->LessThan
                (->Numeric 5)
                (->Add
                 (->Numeric 2)
                 (->Numeric 2)))
               (-reduced {})
               (-reduced {}))))))

(comment
  (str
   (->Add
    (->Multiply (->Numeric 1) (->Numeric 2))
    (->Multiply (->Numeric 3) (->Numeric 4))))

  (str
   (->Add
    (->Add
     (->Multiply (->Numeric 1) (->Numeric 2))
     (->Multiply (->Numeric 3) (->Numeric 4)))
    (->Multiply (->Numeric 3) (->Numeric 4))))

;problem to be fixed =>"(1 * ((2 + 3) * 4))"
;it should bet "(1 * 2 )+ (3 * 4)"
  (str
   (->Multiply (->Numeric 1)
               (->Multiply (->Add (->Numeric 2)
                                  (->Numeric 3))
                           (->Numeric 4))))

  (reduced (reduced (->Add (->Add
                            (->Numeric 1) (->Numeric 3))
                           (->Numeric 3))))

  (reduced (->Multiply (->Numeric 1) (->Numeric 2)))

  (reduced (->Add
            (->Multiply (->Numeric 1) (->Numeric 2))
            (->Multiply (->Numeric 3) (->Numeric 4))))

;
  )
