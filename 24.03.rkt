#lang racket


(define empty-set
  (lambda (x) #f))


(define (singleton a)
  (lambda (x) (eq? x a)))

(define (in a s)
  (s a))

(define (union s z)
  (lambda (x)
    (or (in x s) (in x z))))

(define (intersect s z)
  (lambda (x)
    (and (in x s) (in x z))))


;---------------------
; n + 0            = 0 + n            = n
; (append xs null) = (append null xs) = xs
; m + (n + k)                = (m + n) + k
; (append xs (append ys zs)) = (append (append xs ys) zs)
; 
; (append xs ys) =/= (append ys xs)
; (append '(1 2) '(3 4)) =/= (append '(3 4) '(1 2))
;--------------------
(define (append xs ys)
  (if (null? xs)
      ys
      (cons (car xs) (append (cdr xs) ys))))

(define (list->llist xs)
  (lambda (ys)
    (append xs ys)))

(define (llist->list f)
  (f null))

(define llist-null
  (lambda (ys) ys))

(define (llist-singleton x)
   (lambda (ys) (cons x ys)))

(define (llist-append f g)
  (compose f g))


(define (foldr-reverse xs)
   (foldr (lambda (y ys) (append ys (list y)))
          null
          xs))

(define (foldr-llist-reverse xs)
   (llist->list (foldr (lambda (y ys)
            (llist-append ys (llist-singleton y)))
          llist-null
          xs)))

;--------------------


; x :: Int
(define x 5)

; neg :: Bool -> Bool
(define (neg x)
  (if x #f #t))

; or :: (Bool, Bool) -> Bool

; length :: forall a. [a] -> Int

; identity :: forall a. a -> a

; foldr vs foldl

; foldr
; (a1 # (a2 # ( ... (a(n-1) # (an # b)))))

; foldl
; (((b # a1) # ... # a(n-1)) # an)

; foldr :: ( (a,b) -> b, b, [a]) -> b
; foldl :: ( (b,a) -> b, b, [a]) -> b

;-----------------------
; https://docs.racket-lang.org/guide/define-struct.html
(struct posn (x y) #:transparent)
;> (posn 1 2)
;(posn 1 2)

(struct posn2 (x y))
;> (posn2 1 2)
;#<posn2>


; -----------------
; zad 3
; (lambda (x) x) == identity

;(( lambda ( x ) ( x x ) ) ( lambda ( x ) x ) )
; ==
; ((lambda (x) x) (lambda (x) x))
; ==
; (lambda (x) x)


; (lambda (x) (x x)) == ω
; (ω ω) == Ω
; ((lambda (x) (x x)) (lambda (x) (x x))) == Ω
; == przemianowanie zmiennej x na y
; ((lambda (x) (x x)) (lambda (y) (y y))) == Ω
; == obliczenie
; ((lambda (y) (y y)) (lambda (y) (y y))) == Ω
; == przemianowanie zmiennej y na z
; ((lambda (z) (z z)) (lambda (y) (y y))) == Ω
; == obliczenie
; ((lambda (y) (y y)) (lambda (y) (y y))) == Ω

; https://pl.wikipedia.org/wiki/Operator_paradoksalny
; (lambda (f) ((lambda (x) (f (x x))) (lambda (x) (f (x x))))) == Y

; Y f = ((lambda (x) (f (x x))) (lambda (x) (f (x x))))
;  == obliczenie
; (f ((lambda (x) (f (x x))) (lambda (x) (f (x x)))))
;  == obliczenie
; (f (f ((lambda (x) (f (x x))) (lambda (x) (f (x x))))))
; == n obliczen
; (f ... (f ((lambda (x) (f (x x))) (lambda (x) (f (x x))))))

(define (Y f)
  (lambda (f)
    ((lambda (x) (f (x x))) (lambda (x) (f (x x))))))

(define (factorial n)
  (if (= n 0)
      1
      (* n (factorial (- n 1)))))

(define (fact f n)
  (if (= n 0)
      1
      (* n (f (- n 1)))))

; (Y fact) ~ factorial

