#lang plait

( define-type ( NNF 'v )
   ( nnf-lit [ polarity : Boolean ] [ var : 'v ])
   ( nnf-conj [ l : ( NNF 'v ) ] [ r : ( NNF 'v ) ])
   ( nnf-disj [ l : ( NNF 'v ) ] [ r : ( NNF 'v ) ]))

(define (eval-nnf [σ : ('v -> Boolean)] [φ : (NNF 'v)]) : Boolean
  (type-case (NNF 'v) φ
    [(nnf-lit p v)
     (if p
         (σ v)
         (not (σ v)))]
    [(nnf-conj l r)
     (and (eval-nnf σ l) (eval-nnf σ r))]
    [(nnf-disj l r)
     (or (eval-nnf σ l) (eval-nnf σ r))]))

(define f-false
  (nnf-conj (nnf-lit #t "a") (nnf-lit #f "a")))
(define f-true
  (nnf-disj (nnf-lit #t "a") (nnf-lit #f "a")))
(define σ
  (lambda (x) #t))

(eval-nnf σ f-true)
(eval-nnf σ f-false)

( define-type ( Formula 'v )
   ( var [ var : 'v ])
   ( neg [ f : ( Formula 'v ) ])
   ( conj [ l : ( Formula 'v ) ] [ r : ( Formula 'v ) ])
   ( disj [ l : ( Formula 'v ) ] [ r : ( Formula 'v ) ]) )

(define (eval-formula [σ : ('v -> Boolean)] [φ : (Formula 'v)]) : Boolean
  (type-case (Formula 'v) φ
    [(var v)
     (σ v)]
    [(neg f)
     (not (eval-formula σ f))]
    [(conj l r)
     (and (eval-formula σ l) (eval-formula σ r))]
    [(disj l r)
     (or (eval-formula σ l) (eval-formula σ r))]))

;(define (to-nnf [formula : (Formula 'v)]) : (NNF 'v)
;  (local
;    [(define (aux [p : Boolean] [formula : (Formula 'v)])
;       (type-case (Formula 'v) formula
;         [(var v) (nnf-lit p v)]
;         [(neg f) (aux (not p) f)]
;         [(conj l r)
;          (if p
;              (nnf-conj (aux p l) (aux p r))
;              (nnf-disj (aux p l) (aux p r)))]
;         [(disj l r)
;          (if p
;              (nnf-disj (aux p l) (aux p r))
;              (nnf-conj (aux p l) (aux p r)))]))]
;    (aux #t formula)))

(define (to-nnf [formula : (Formula 'v)]) : (NNF 'v)
   (type-case (Formula 'v) formula
     [(var v) (nnf-lit #t v)]
     [(neg f) (neg-nnf (to-nnf f))]
     [(conj l r)
      (nnf-conj (to-nnf l) (to-nnf r))]
     [(disj l r)
      (nnf-disj (to-nnf l) (to-nnf r))]))

; ∀ σ,φ . (eval-formula σ φ) ~ (eval-nnf σ (to-nnf φ))
; L = (eval-formula σ φ)
; P = (eval-nnf σ (to-nnf φ))

; indukcja po φ

; 1) φ := (var v)
;    L =
;    {z definicji eval-formula}
;    = (σ v)

;    P =
;    {z definicji to-nnf}
;    = (eval-nnf σ (nnf-lit #t v))
;    {z definicji eval-nnf}
;    = (σ v)
;    L = P

; 2) φ := (neg f)
;    HI := ∀ σ. (eval-formula σ f) ~ (eval-nnf σ (to-nnf f))
;
;    L =
;    {z definicji eval-formula}
;    = (not (eval-formula σ f))
;    {z HI}
;    = (not (eval-nnf σ (to-nnf f)))

;    P =
;    {z definicji to-nnf}
;    = (eval-nnf σ (neg-nnf (to-nnf f)))
;    {z definicji neg-nnf}
;    .... 

; 3) φ := (conj l r)
;    HI1 := ∀ σ. (eval-formula σ l) ~ (eval-nnf σ (to-nnf l))
;    HI2 := ∀ σ. (eval-formula σ r) ~ (eval-nnf σ (to-nnf r))
;
;    L =
;    {z definicji eval-formula}
;    = (and (eval-formula σ l) (eval-formula σ r))
;    {z HI1}
;    = (and (eval-nnf σ (to-nnf l)) (eval-formula σ r))
;    {z HI2}
;    = (and (eval-nnf σ (to-nnf l)) (eval-nnf σ (to-nnf r))

;    P =
;    {z to-nnf}
;    = (eval-nnf σ (nnf-conj (to-nnf l) (to-nnf r)))
;    {z def eval-nnf}
;    =  (and (eval-nnf σ (to-nnf l)) (eval-nnf σ (to-nnf r)))
;
;    L = P