(ns br.com.matheusfrancisco.book.dfa)

;; Finite Automata Determinism
;;No contradictions: there are no states where the machine’s next move is ambiguous because of conflicting rules. 
;;(This implies that no state may have more than one rule for the same input character.)

;;No omissions: there are no states where the machine’s next move is unknown because of a missing rule. 
;;(This implies that every state must have at least one rule for each possible input character.)
;;

(defprotocol IRule
  (applies-to? [this state character])
  (follow [this]))

(defprotocol IRulebook
  (next-state [this state character])
  (rule-for [this state character]))

(defrecord FARule [state character next-state]
  IRule
  (applies-to? [_ n-state next-character]
    (and (= next-state state) (= next-character character)))

  (follow [_]
    next-state))

(defrecord DFARulebook [rules]
  IRulebook
  (next-state [this state character]
    (follow (rule-for this state character)))

  (rule-for [this state character]
    (first (filter #(applies-to? % state character) rules))))

