#lang racket

(define (insert x xs)
  (if (null? xs)
      (list x)
      (let ((a (car xs)))
        (if (<= x a)
            (cons x xs)
            (cons a (insert x (cdr xs)))))))

(define (sort l)
  (define (aux l result)
    (if (null? l)
        result
        (aux (cdr l) (insert (car l) result))))
  (aux l '()))


(define (quick xs)
  (define (split pivot xs l g)
    (if (null? xs)
	; elementy w listach l, g sa odwrocone, ale nam to nie przeszkadza
        (cons l g)
        (if (<= pivot (car xs))
            (split pivot (cdr xs) l (cons (car xs) g))
            (split pivot (cdr xs) (cons (car xs) l) g))))
  
  (define (aux pivot xs)
    (let ((s (split pivot xs '() '())))
      (let ((le (quick (car s)))
            (gr (quick (cdr s))))
      ; To wcale takie szybkie nie jest bo append dziala w czasie liniowym do dlugosci listy le
      (append le (list pivot) gr))))
  
  (cond [(null? xs) xs]
        [(null? (cdr xs)) xs]
        [else (aux (car xs) (cdr xs))]))


(define (fst x y) x)
(define fst2
  (lambda (x y) x))


(define (matrix a b c d)
  (lambda (row col)
    (cond [(and (= col 0) (= row 0))
           a]
          [(and (= col 0) (= row 1))
           c]
          [(and (= col 1) (= row 0))
           b]
          [(and (= col 1) (= row 1))
           d])))

; https://docs.racket-lang.org/reference/match.html
