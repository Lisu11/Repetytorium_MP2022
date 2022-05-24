#lang plait

;   +
;  /  \
; 2     *
;      /   \
;     3      -
;           / \
;          7   21
; 2 + 3 * (7 - 21)

(define-type Op
  (op-add) (op-sub) (op-mul) (op-div) (op-pow))

(define-type Unary
  (op-fact))

(define-type Exp
  (exp-number [n : Number])
  (exp-unary [op : Unary] [e : Exp])
  (exp-op [op : Op] [e1 : Exp] [e2 : Exp]))
