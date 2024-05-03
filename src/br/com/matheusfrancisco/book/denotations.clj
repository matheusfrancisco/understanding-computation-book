(ns br.com.matheusfrancisco.book.denotations)

(defprotocol Expression
  (to->clj [this]))

(defn invoke
  ([f] (invoke f {}))
  ([f e] (f e)))

(defn exec [f e]
  (invoke (eval (to->clj f)) e))

(defrecord Numeric [value]
  Expression
  (to->clj [_]
    `(fn [_#] ~value)))

(defrecord Bool [value]
  Expression
  (to->clj [_]
    `(fn [_#] ~value)))

(defrecord Variable [varname]
  Expression
  (to->clj [_]
    `(fn [env#] (get env# ~varname))))

; Expressions
(defrecord Add [left right]
  Expression
  (to->clj [_]
    `(fn [environment#]
       (+ (-> ~left to->clj eval (invoke environment#))
          (-> ~right to->clj eval (invoke environment#))))))

(defrecord Multiply [left right]
  Expression
  (to->clj [_]
    `(fn [environment#]
       (* (-> ~left to->clj eval (invoke environment#))
          (-> ~right to->clj eval (invoke environment#))))))

(defrecord LessThan [left right]
  Expression
  (to->clj [_]
    `(fn [environment#]
       (< (-> ~left to->clj eval (invoke environment#))
          (-> ~right to->clj eval (invoke environment#))))))

; Statements
(defrecord DoNothing []
  Expression
  (to->clj [_]
    `(fn [environment#]
       environment#)))

(defrecord Assign [varname expression]
  Expression
  (to->clj [_]
    `(fn [environment#]
       (assoc environment# ~varname
              (exec ~expression environment#)))))

(defrecord If [condition consequence alternative]
  Expression
  (to->clj [_]
    `(fn [environment#]
       (if (exec ~condition environment#)
         (exec ~consequence environment#)
         (exec ~alternative environment#)))))

(defrecord Sequence [first-seq second-seq]
  Expression
  (to->clj [_]
    `(fn [environment#]
       (exec ~second-seq (exec ~first-seq environment#)))))

(defrecord While [condition body]
  Expression
  (to->clj [_]
    `(fn [environment#]
       (if (exec ~condition environment#)
         (recur (exec ~body environment#))
         (exec (->DoNothing) environment#)))))


