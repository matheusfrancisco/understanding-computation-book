(ns br.com.matheusfrancisco.book.core
  (:require [br.com.matheusfrancisco.book.small-steps :refer [machine-run
                                                              ->While
                                                              ->LessThan
                                                              ->Variable
                                                              ->Numeric
                                                              ->Assign
                                                              ->Add
                                                              ->Sequence]]))

(machine-run
 (->While
  (->LessThan (->Variable :x) (->Numeric 5))
  (->Assign :x (->Add (->Variable :x) (->Numeric 1))))
 {:x (->Numeric 1)})
