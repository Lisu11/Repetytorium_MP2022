#lang plait

(define-type RoseTree
  (leaf [v : String])
  (children [c : (Listof RoseTree)]))

(define (inorder [t : RoseTree]) : (Listof String)
  (local
    [(define (list-append [l : (Listof (Listof String))]) : (Listof String)
       (if (empty? l)
           '()
           (append (first l) (list-append (rest l)))))]
  
    (type-case RoseTree t
      [(leaf value) (list value)]
      [(children c)
       ; map : (a -> b) (Listof a) -> (Listof b)
       ; map : (RoseTree -> (Listof String)) (Listof RoseTree) -> (Listof (Listof String))
       (list-append (map inorder c))])))



(define-type Prop
  (var [v : String])
  (conj [l : Prop] [r : Prop])
  (disj [l : Prop] [r : Prop])
  (neg [f : Prop])
  (impl [l : Prop] [r : Prop]))

;((x /\ ~y) \/ z) -> (x \/ ~x)
(define p : Prop
  (impl (disj (conj (var "x") (neg (var "y")))
              (var "z"))
        (disj (var "x") (neg (var "x")))))


(define (free-vars [p : Prop]) : (Listof String)
  (local
    [(define (aux [p : Prop] [l : (Listof String)]) : (Listof String)
       (type-case Prop p
         [(var x) (if (member x l)
                      l
                      (cons x l))]
         [(conj left right)  (aux left (aux right l))]
         [(disj left right)  (aux left (aux right l))]
         [(impl left right)  (aux left (aux right l))]
         [(neg t)  (aux t l)]))]
    (aux p '())))
 

(define (eval [h : (Hashof String Boolean)] [p : Prop]) : Boolean
  (type-case Prop p
    [(var x)
     (type-case (Optionof Boolean) (hash-ref h x)
       [(none) (error 'val  "h nie jest waluacja")]
       [(some bool) bool])]
    [(conj left right) (and (eval h left)
                            (eval h right))]
    [(disj left right) (or (eval h left)
                           (eval h right))]
    [(impl left right) (or (not (eval h left))
                           (eval h right))]
    [(neg t) (not (eval h t))]))


(define (tautology? [p : Prop]) : Boolean
  (local
    [(define (all [pred : ((Hashof String Boolean) -> Boolean)]
                  [vals : (Listof (Hashof String Boolean))]) : Boolean
       (if (empty? vals)
           #t
           (and (pred (first vals)) (all pred (rest vals)))))
     
     (define (valuations [l : (Listof String)]) : (Listof (Hashof String Boolean))
       (local
         [(define (aux [l : (Listof String)]
                       [h : (Hashof String Boolean)]) : (Listof (Hashof String Boolean))
         (if (empty? l)
             (list h)
             (append (aux (rest l) (hash-set h (first l) #t))
                     (aux (rest l) (hash-set h (first l) #f)))))]
       (aux l (hash '()))))]
    (let ([vals (valuations (free-vars p))])
      (all (lambda (v) (eval v p)) vals))))
