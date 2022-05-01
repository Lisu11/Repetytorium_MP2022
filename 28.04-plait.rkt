#lang plait

(define-type Tree
  (leaf)
  (node-2 [l : Tree] [v : Number] [r : Tree])
  (node-3 [l : Tree] [v1 : Number] [c : Tree] [v2 : Number] [r : Tree]))


; Exists x. P(x)
; 
; ~ Exists x. P(x) -> False = Forall x. P(x) -> False

; x' taki ze  P(x')

(define-type Tree-Aux
  (aux-3 [l : Tree] [v1 : Number] [c : Tree] [v2 : Number] [r : Tree])
  (aux-2 [l : Tree] [v : Number] [r : Tree])
  (aux-2-bigger [l : Tree] [v : Number] [r : Tree]))

; (define (insert-aux [t : Tree] [v : Number]) : Tree-Aux


(define (insert [t : Tree] [v : Number]) : Tree
  (if (leaf? t) 
      (node-2 (leaf) v (leaf))
      (type-case Tree-Aux (insert-aux t v)
        [(aux-3 l w1 c w2 r)
         (node-3 l w1 c w2 r)]
        [(aux-2 l w r)
         (node-2 l w r)]
        [(aux-2-bigger l w r)
         (node-2 l w r)])))

