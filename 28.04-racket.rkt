#lang racket
; map : (a -> b), [a] -> [b]
; append-map : ('a -> ['b]), ['a] -> ['b]
; 'a := [a]
; 'b := [a]
; append-map : ([a] -> [[a]]),   [[a]] -> [[a]]
; [a] -> ([a], [a])
; sublist (1 2 3)
; ~> ((2 3) (2) (3) ()) = sublist (2 3)
;    ((2 3) (2) (3) ()) ++ ( (1 2 3) (1 2) (1 3) (1) )

; sublist : [a] -> [[a]]
(define (sublists xs) 
  (if (null? xs)
      (list null) ; : [[a]]
      (append-map
       (lambda ( ys ) ; ys : [a]
         (cons (cons (car xs) ys) ; (cons (car xs) ys) : [a]
               (list ys))) ; 
       (sublists (cdr xs)) ; : [[a]]
       )))

; zip : [a],[b] -> [(a,b)]
; zip (1 2) (a b c) = [(1 a) (2 b)]
; zip la lb = []


; [1 2 3]
; ["a" "b" "c"]
; [('a . 1) ('a . 2) ('b . 3)]
(define/contract (suffixes str)
  (parametric->/c [a] (-> (listof a) (listof (listof a))))
  (if (null? str)
      (list null)
      (append (list str) (suffixes (cdr str)))))

( parametric- >/ c [ a b c ] (-> (-> a b c ) (-> a b ) a c ) )