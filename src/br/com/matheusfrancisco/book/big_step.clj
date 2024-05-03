(ns br.com.matheusfrancisco.book.big-step)

;The small-step approach has the advantage of slicing up the complex business of executing an 
;entire program into smaller pieces that are easier to explain and analyze, but it does feel a 
;bit indirect: instead of explaining how a whole program construct works, we just show how it can be reduced slightly.
;

;big-step semantics is often written in a looser style that just says which subcomputations to perform
;without necessarily specifying what order to perform them in.[14]

(defprotocol Expressions
  (evaluate [this env]))

(defrecord Numeric [value]
  Expressions
  (evaluate [this _] this))

(defrecord Bool [value]
  Expressions
  (evaluate [this _] this))

(defrecord Variable [varname]
  Expressions
  (evaluate [this env] (get env varname)))

(defrecord Add [left right]
  Expressions
  (evaluate [this env]
    (let [left-value (evaluate left env)
          right-value (evaluate right env)]
      (->Numeric (+ (.value left-value) (.value right-value))))))

(defrecord Multiply [left right]
  Expressions
  (evaluate [this env]
    (let [left-value (evaluate left env)
          right-value (evaluate right env)]
      (->Numeric (* (.value left-value) (.value right-value))))))

(defrecord LessThan [left right]
  Expressions
  (evaluate [this env]
    (let [left-value (evaluate left env)
          right-value (evaluate right env)]
      (->Bool (< (.value left-value) (.value right-value))))))

; Statments

(defrecord Assign [varname expression]
  Expressions
  (evaluate [this env]
    (let [value (evaluate expression env)]
      (assoc env varname value))))

(defrecord DoNothing []
  Expressions
  (evaluate [this env] env))

(defrecord If [condition consequence alternative]
  Expressions
  (evaluate [this env]
    (let [condition-value (evaluate condition env)]
      (condp = condition-value
        (->Bool true) (evaluate consequence env)
        (->Bool false) (evaluate alternative env)))))

(defrecord Sequences [first-sq second-sq]
  Expressions
  (evaluate [this env]
    (evaluate second-sq (evaluate first-sq env))))

(defrecord While [condition body]
  Expressions
  (evaluate [this env]
    (let [condition-value (evaluate condition env)]
      (condp = condition-value
        (->Bool true) (evaluate this (evaluate body env))
        (->Bool false) env))))
