(ns br.com.matheusfrancisco.book.big-step)

;The small-step approach has the advantage of slicing up the complex business of executing an 
;entire program into smaller pieces that are easier to explain and analyze, but it does feel a 
;bit indirect: instead of explaining how a whole program construct works, we just show how it can be reduced slightly.
;

;big-step semantics is often written in a looser style that just says which subcomputations to perform
;without necessarily specifying what order to perform them in.[14]

(defprotocol Expression
  (evaluate [this env]))

(defrecord Numeric [value]
  Expression
  (evaluate [this _] this))

(defrecord Bool [value]
  Expression
  (evaluate [this _] this))

(defrecord Variable [varname]
  Expression
  (evaluate [this env] (get env varname)))

(defrecord Add [left right]
  Expression
  (evaluate [this env]
    (let [left-value (evaluate left env)
          right-value (evaluate right env)]
      (->Numeric (+ (.value left-value) (.value right-value))))))

(defrecord Multiply [left right]
  Expression
  (evaluate [this env]
    (let [left-value (evaluate left env)
          right-value (evaluate right env)]
      (->Numeric (* (.value left-value) (.value right-value))))))

(defrecord LessThan [left right]
  Expression
  (evaluate [this env]
    (let [left-value (evaluate left env)
          right-value (evaluate right env)]
      (->Bool (< (.value left-value) (.value right-value))))))

;3 < 4
(evaluate (->LessThan (->Numeric 3) (->Numeric 4)) {}) ;{:value true}

(evaluate (->Multiply (->Numeric 10) (->Numeric 2)) {}) ;{:value 20}
(evaluate (->Add (->Numeric 1) (->Numeric 2)) {}) ;{:value 3}

(evaluate (->Numeric 1) {})

; Statments

(defrecord Assign [varname expression]
  Expression
  (evaluate [this env]
    (let [value (evaluate expression env)]
      (assoc env varname value))))

(defrecord DoNothing []
  Expression
  (evaluate [this env] env))

(defrecord If [condition consequence alternative]
  Expression
  (evaluate [this env]
    (let [condition-value (evaluate condition env)]
      (condp = condition-value
        (->Bool true) (evaluate consequence env)
        (->Bool false) (evaluate alternative env)))))

#_(defrecord Sequences [first-sq second-sq]
    (evaluate [this env]
      (let [first-env (evaluate first-sq env)]
        (evaluate second-sq first-env))))

; x = 1 + 1; y = x + 3
#_(evaluate (->Sequences
             (->Assign :x (->Add (->Numeric 1) (->Numeric 1)))
             (->Assign :y (->Add (->Variable :x) (->Numeric 3)))) {})

#_(defrecord While [condition body]
    (evaluate [this env]
      (let [condition-value (evaluate condition env)]
        (condp = condition-value
          (->Bool true) (evaluate this (evaluate body env))
          (->Bool false) env))))

#_(evaluate (->If (->Bool true)
                  (->Assign :x (->Numeric 1))
                  (->Assign :y (->Numeric 2))) {})
#_(evaluate (->While
             (->LessThan (->Variable :x) (->Numeric 5))
             (->Assign :x (->Add (->Variable :x) (->Numeric 1))))
            {:x (->Numeric 1)})

;{:x {:value 1}}
