#lang plait

( define-type ( Formula 'v )
   ( var [ var : 'v ])
   ( neg [ f : ( Formula 'v ) ])
   ( conj [ l : ( Formula 'v ) ] [ r : ( Formula 'v ) ])
   ( disj [ l : ( Formula 'v ) ] [ r : ( Formula 'v ) ]) )

( define-type ( NNF 'v )
   ( nnf-lit [ polarity : Boolean ] [ var : 'v ])
   ( nnf-conj [ l : ( NNF 'v ) ] [ r : ( NNF 'v ) ])
   ( nnf-disj [ l : ( NNF 'v ) ] [ r : ( NNF 'v ) ]) )

(define (to-nnf [formula : (Formula 'v)]) : (NNF 'v)
  (local
    [(define (aux [p : Boolean] [formula : (Formula 'v)])
       (type-case (Formula 'v) formula
         [(var v) (nnf-lit p v)]
         [(neg f) (aux (not p) f)]
         [(conj l r)
          (if p
              (nnf-conj (aux p l) (aux p r))
              (nnf-disj (aux p l) (aux p r)))]
         [(disj l r)
          (if p
              (nnf-disj (aux p l) (aux p r))
              (nnf-conj (aux p l) (aux p r)))]))]
    (aux #t formula)))

(to-nnf (neg (disj (var "x") (neg (var "y")))))