(ns br.com.matheusfrancisco.book.core)

;; small-step semantics
;; (1 * 2) + (3 * 4)
;; perform the left hand multiplication and reduce the expression to
;; 2 + (3 * 4)
;; perform the right hand multiplication and reduce the expression to
;; 2 + 12
;; perform the addition (2 + 12 becames 14) and end up with 14 

;;reduce
;If the addition’s left argument can be reduced, reduce the left argument.
;If the addition’s left argument can’t be reduced but its right argument can, reduce the right argument.
;If neither argument can be reduced, they should both be numbers, so add them together.;

(defprotocol Reducible
  (reducible? [this])
  (-reduced [this env]))

(defrecord Numeric [value]

  Reducible
  (reducible? [_] false)
  (-reduced [this _e] this)

  Object
  (toString [_] (format "%s" value)))

;; Operations
(defrecord Add [left right]

  Reducible
  (reducible? [_] true)

  (-reduced [_ env]
    (cond
      (reducible? left) (->Add (-reduced left env) right)
      (reducible? right) (->Add left (-reduced right env))
      :else (->Numeric (+ (.value left) (.value right)))))

  Object
  (toString [_] (format "(%s + %s)" left right)))

(defrecord Multiply [left right]
  Reducible
  (reducible? [_] true)

  (-reduced [_ env]
    (cond
      (reducible? left) (->Multiply (-reduced left env) right)
      (reducible? right) (->Multiply left (-reduced right env))
      :else (->Numeric (* (:value left) (:value right)))))

  Object
  (toString [_] (format "(%s * %s)" left right)))

(defrecord Bool [value]
  Reducible
  (reducible? [_] false)
  (-reduced [this _] (str (:value this)))

  Object
  (toString [_] (format "%s" value)))

(defrecord LessThan [left right]
  Reducible
  (reducible? [_] true)

  (-reduced [_ env]
    (cond
      (reducible? left) (->LessThan (-reduced left env) right)
      (reducible? right) (->LessThan left (-reduced right env))
      :else (->Bool (< (:value left) (:value right)))))

  Object
  (toString [_] (format "%s < %s" left right)))

; Variable
(defrecord Variable [n]
  Reducible
  (reducible? [_] true)
  (-reduced [this env] ((:n this) env))

  Object
  (toString [_] (format "%s" (name n))))

;; statements

(defrecord DoNothing []
  Reducible
  (reducible? [_] false)
  (-reduced [this _] this)

  Object
  (toString [_] "do-nothing"))

(defrecord Assign [varname expression]
  Reducible
  (reducible? [_] true)

  (-reduced [this env]
;To summarize, the reduction rules for assignment are:
;If the assignment’s expression can be -reduced, then reduce it, resulting in a reduced assignment statement and an unchanged environment.
;If the assignment’s expression can’t be -reduced, then update the environment 
;to associate that expression with the assignment’s variable, resulting in a «do-nothing» statement and a new environment.
    (if (reducible? (:expression this))
      [(->Assign varname (-reduced expression env)) env]
      [(->DoNothing) (assoc env varname expression)]))

  Object
  (toString [this] (format "%s = %s" (name (:varname this)) (:expression this))))

;If the condition can be reduced, then reduce it, resulting in a reduced conditional statement and an unchanged environment.
;If the condition is the expression «true», reduce to the consequence statement and an unchanged environment.
;If the condition is the expression «false», reduce to the alternative statement and an unchanged environment.

(defrecord If [condition consequence alternative]

  Reducible
  (reducible? [_] true)
  (-reduced [_ env]
    (if (reducible? condition)
      [(->If (-reduced condition env)
             consequence
             alternative) env]
      (let [value (:value condition)]
        (if value
          [consequence env]
          [alternative env]))))

  Object
  (toString [_]
    (format "if (%s) { %s } else { %s }"
            condition
            consequence
            alternative)))

;;The reduction rules for sequences are slightly subtle:
;;If the first statement is a «do-nothing» statement, reduce to the second statement and the original environment.
;;If the first statement is not «do-nothing», then reduce it, resulting in a new sequence 
;;(the reduced first statement followed by the second statement) and a reduced environment.

;;x = 1 + 1; x = y + 3
(defrecord Sequence [first-seq second-seq]
  Reducible
  (reducible? [_] true)

  (-reduced [_ env]
    (cond
      (= first-seq (->DoNothing)) [second-seq env]
      :else (let [[reduced-f reduced-env] (-reduced first-seq env)]
              [(->Sequence reduced-f second-seq) reduced-env])))

  Object
  (toString [_] (format "%s; %s" first second)))

;;while (x < 5) { x = x + 1 }
;;Perhaps this reduction rule seems like a bit of a dodge—it’s almost as though we’re perpetually postponing reduction 
;;of the «while» until later, without ever actually getting there—but on the other hand, it does 
;;a good job of explaining what we really mean by a «while» statement: check the condition, 
;;evaluate the body, then start again. It’s curious that reducing «while» turns it into a syntactically 
;;larger program involving conditional and sequence statements instead of directly reducing its condition 
;;or body, and one reason why it’s useful to have a technical framework for specifying the formal 
;;semantics of a language is to help us see how different parts of the language relate to each other like this.
;;We’ve completely ignored what will happen when a syntactically valid but otherwise incorrect 
;;program is executed according to the semantics we’ve given. The statement «x = true; x = x + 1»
;;is a valid piece of Simple syntax—we can certainly construct an abstract syntax tree to represent it—but it’ll
;;blow up when we try to repeatedly reduce it, because the abstract machine will end up trying to add «1» to «true»:
(defrecord While [condition body]
  Reducible
  (reducible? [_] true)

  (-reduced [this env]
    [(->If condition
           (->Sequence body this)
           (->DoNothing)) env])

  Object
  (toString [_] (format "while (%s) { %s }" condition body)))

(defn machine-run [stmt env]
  (if (reducible? stmt)
    (let [[s1 e1] (-reduced stmt env)]
      (machine-run s1 e1))
    [stmt env]))

(comment
  (machine-run
   (->While
    (->LessThan (->Variable :x) (->Numeric 5))
    (->Assign :x (->Add (->Variable :x) (->Numeric 1))))
   {:x (->Numeric 1)})

  (-> (machine-run
       (->Sequence
        (->Assign :x (->Add (->Numeric 1) (->Numeric 2)))
        (->Assign :y (->Add (->Variable :x) (->Numeric 3))))
       {})
      second)

  (-> (machine-run (->If (->Bool true)
                         (->Assign :x (->Numeric 1))
                         (->Assign :y (->Numeric 2))) {})
      second
      :x)

  (machine-run
   (->If (->Variable :x)
         (->Assign :y (->Numeric 1))
         (->Assign :y (->Numeric 2)))
   {:x (->Bool true)})

  (machine-run
   (->If (->Variable :x)
         (->Assign :y (->Numeric 1))
         (->DoNothing))
   {:x (->Bool false)})

  (str (->If (->Bool true)
             (->Assign :x (->Numeric 1))
             (->Assign :y (->Numeric 2))))
  (machine-run
   (->Assign :x (->Add (->Numeric 1) (->Numeric 2))) {})

  (machine-run (->Assign :x (->Add
                             (->Variable :x)
                             (->Variable :y)))
               {:x (->Numeric 3), :y (->Numeric 4)})

  (str
   (machine-run (->Add
                 (->Variable :x)
                 (->Variable :y))
                {:x (->Numeric 3), :y (->Numeric 4)}))

  (str
   (let [[_ env]
         (machine-run (->Assign :x (->Add (->Numeric 1) (->Numeric 2))) {})]
     (-> env :x))) ;=> 3

;
  )

