#lang racket

;; ZADANIE 2
;; =========

;; W tym zadaniu przyjrzymy się pierwszemu "językowi programowania"
;; który widzieliśmy na zajęciach: wyrażeniom arytmetycznym. Ich
;; prostota przejawia się przede wszystkim tym że nie występują w nich
;; zmienne (a w szczególności ich wiązanie) — dlatego możemy o nich
;; wnioskować nie używając narzędzi cięższych niż te poznane na
;; wykładzie.

;; W tym zadaniu będziemy chcieli udowodnić że nasza prosta kompilacja
;; do odwrotnej notacji polskiej jest poprawna. Konkretniej, należy
;; · sformułować zasady indukcji dla obydwu typów danych
;;   reprezentujących wyrażenia (expr? i rpn-expr?)
;; · sformułować i udowodnić twierdzenie mówiące że kompilacja
;;   zachowuje wartość programu, tj. że obliczenie wartości programu
;;   jest równoważne skompilowaniu go do RPN i obliczeniu.
;; · sformułować i udowodnić twierdzenie mówiące że translacja z RPN
;;   do wyrażeń arytmetycznych (ta która była zadaniem domowym;
;;   implementacja jest poniżej) jest (prawą) odwrotnością translacji
;;   do RPN (czyli że jak zaczniemy od wyrażenia i przetłumaczymy do
;;   RPN i z powrotem, to dostaniemy to samo wyrażenie).
;; Swoje rozwiązanie należy wpisać na końcu tego szablonu w
;; komentarzu, podobnie do niniejszej treści zadania; proszę zadbać o
;; czytelność dowodów!

(struct const (val) #:transparent)
(struct binop (op l r) #:transparent)

(define (operator? x)
  (member x '(+ * - /)))

(define (expr? e)
  (match e
    [(const v)
     (integer? v)]
    [(binop op l r)
     (and (operator? op)
          (expr? l)
          (expr? r))]
    [_ false]))

;; (A /\ B) -> C ~~ A -> (B -> C)

(define (curry f)
  (lambda (a)
    (lambda (b) (f a b))))

(define (uncarry g)
  (lambda (a b)
    ((g a) b)))

; (curry (uncarry g)) ~ g
; (uncurry (curry f)) ~ f

;; 
;; *) forall v. (number? v) -> P (const v)
;; **) forall op, l r. (operator? op) -> P (l) -> P (r) -> P (binop op l r)
;; *) /\ **) -> forall e. (expr? e) -> P (e)


;; forall e. (expr? e) ->
;;           (eval e) ~ (rpn-eval (arith->rpn e))


; P (e) := (eval e) ~ (rpn-eval (arith->rpn e))

; *)
; forall v. (number? v) -> P (const v)
; wezmy dowolne v t z (number? v)
; L = (eval (const v)) = v
; P =  (rpn-eval (arith->rpn (const v))) =
; = (rpn-eval (list v)) =
; = (eval-am (list v) empty-stack) =
; = (eval-am null (stack (list v))) =
; = v
; L = P

; **)
; forall op, l r. (operator? op) -> P (l) -> P (r) -> P (binop op l r)
; wezmy op l r
; (eval l) ~ (rpn-eval (arith->rpn l))
; (eval r) ~ (rpn-eval (arith->rpn r))
; chcemy pokazac ze zachodzi  (eval (binop op l r)) ~ (rpn-eval (arith->rpn (binop op l r)))
; L = (let ((vl (eval l))
;           (vr (eval r))
;           (p  (op->proc op)))
;       (p vl vr)) =
; / z HIr i z HIl/
; = ((op->proc op) (rpn-eval (arith->rpn l))  (rpn-eval (arith->rpn r)))

; P = (rpn-eval (arith->rpn (binop op l r))) =
; = (rpn-eval (append (arith->rpn l) (arith->rpn r) (list op))) =
; = (eval-am (append (arith->rpn l) (arith->rpn r) (list op)) empty-stack)
; /z lematu/ 
; = (eval-am (append (list (rpn-eval (arith->rpn e))) (arith->rpn r) (list op)) empty-stack) =
; = (eval-am (append (arith->rpn r) (list op)) (stack  (list (rpn-eval (arith->rpn l))))
; / z lematu/
; = (eval-am (append (list (rpn-eval (arith->rpn r)) (list op)) (stack  (list (rpn-eval (arith->rpn l))))
; = (eval-am (list op)  (stack  (list (rpn-eval (arith->rpn r)) (rpn-eval (arith->rpn l))))
; = (let* ((vr (top s))
;           (s  (pop s))
;           (vl (top s))
;           (s  (pop s))
;           (v  ((op->proc (car e)) vl vr)))
;      (eval-am (cdr e) (push v s)))
; = (eval-am null (stack  (list ((op->proc op) (rpn-eval (arith->rpn r)) (rpn-eval (arith->rpn l)))))
; = ((op->proc op) (rpn-eval (arith->rpn r)) (rpn-eval (arith->rpn l)))

; L = P

; lemat : forall s.  (eval-am (append (arith->rpn e) ...) s) ~ (eval-am (append (list (rpn-eval (arith->rpn e))) ...)) s)
; Proof: TODO
;-------------------------------------------------------------
;czyli że jak zaczniemy od wyrażenia i przetłumaczymy do
;;   RPN i z powrotem, to dostaniemy to samo wyrażenie).

;; forall e. (expr? e) ->
;;          (rpn->arith (arith->rpn e)) ~ e

(define (value? v)
  (number? v))

(define (op->proc op)
  (match op
    ['+ +]
    ['- -]
    ['* *]
    ['/ /]))

;; zał: (expr? e) jest prawdą
;; (value? (eval e)) jest prawdą
(define (eval e)
  (match e
    [(const v) v]
    [(binop op l r)
     (let ((vl (eval l))
           (vr (eval r))
           (p  (op->proc op)))
       (p vl vr))]))

(define (rpn-expr? e)
  (and (list? e)
       (pair? e)
       (andmap (lambda (x) (or (number? x) (operator? x))) e)))


(struct stack (xs))

(define empty-stack (stack null))
(define (empty-stack? s) (null? (stack-xs s)))
(define (top s) (car (stack-xs s)))
(define (push a s) (stack (cons a (stack-xs s))))
(define (pop s) (stack (cdr (stack-xs s))))


(define (eval-am e s)
  (cond
   [(null? e)            (top s)]
   [(number? (car e))    (eval-am (cdr e) (push (car e) s))]
   [(operator? (car e))
    (let* ((vr (top s))
           (s  (pop s))
           (vl (top s))
           (s  (pop s))
           (v  ((op->proc (car e)) vl vr)))
      (eval-am (cdr e) (push v s)))]))

(define (rpn-eval e)
  (eval-am e empty-stack))

(define (arith->rpn e)
  (match e
    [(const v)      (list v)]
    [(binop op l r) (append (arith->rpn l) (arith->rpn r) (list op))]))

(define (rpn-translate e s)
  (cond
   [(null? e)
    (top s)]

   [(number? (car e))
    (rpn-translate (cdr e) (push (const (car e)) s))]

   [(operator? (car e))
    (let* ((er (top s))
           (s  (pop s))
           (el (top s))
           (s  (pop s))
           (en (binop (car e) el er)))
      (rpn-translate (cdr e) (push en s)))]))

(define (rpn->arith e)
  (rpn-translate e empty-stack))

