#lang racket

;findf format modeled from a post on the d2jsp.org forum by J_B
(define symfind
  (lambda (symbol table)
    (let ([lookup (findf (lambda (c) (equal? symbol (first c))) table)])
      (if lookup (cadr lookup) symbol))))

; an operation can appear as quoted or as a procedure, depending on how the 
; expression was constructed, so detect both forms.

(define (is-plus? op) (or (equal? op +) (equal? op '+))) 
(define (is-times? op) (or (equal? op *) (equal? op '*)))
(define (is-minus? op) (or (equal? op -) (equal? op '-))) 
(define (is-divide? op) (or (equal? op /) (equal? op '/)))

; apply op to left and right arguments, assume they are numbers
; this can be tested with the number? predicate
(define (op-eval op larg rarg)
  (if (and (number? larg) (number? rarg))
      (cond 
        [ (is-plus? op) (+ larg rarg) ]
        [ (is-times? op) (* larg rarg) ]
        [ (is-minus? op) (- larg rarg) ]
        [ (is-divide? op) (/ larg rarg) ]
        )
      (list larg op rarg)))

; evaluate an infix expression tree, assuming that all the leaves 
; are numbers.
(define (exp-eval ex [symtable '()])
  (cond
    ; atoms are themselves
    [ (not (list? ex)) (symfind ex symtable) ]  
    
    ; single element lists are the value of their first element
    [ (null? (rest ex)) (exp-eval (first ex) symtable) ] 
    
    ; triples require evaluation of left and right, followed by operation
    [ else (op-eval (second ex) (exp-eval (first ex) symtable) (exp-eval (third ex) symtable)) ]
    ))

(exp-eval 1)
(exp-eval '1)
(exp-eval '(1) '())
(exp-eval '(1 + 2) '())
(exp-eval '(3 * 4) )
(exp-eval '( (1 + 2) * (3 + 4) ) '())
(exp-eval '(1 + x) '())
(exp-eval '(1 + (5 * 3) + (9 * 3) ) '( (x 2) (c 6) ) )

(define base-equal-expanded
  (lambda (e1o e1larg e1rarg e2o e2larg e2rarg)
    ;assume that this is the base case
    (cond
      ;Operations are both either + or *
      [(and (equal? e1o e2o) (or (is-plus? e1o) (is-times? e1o))) (if
       (or (and (equal? e1larg e2larg) (equal? e1rarg e2rarg))
           (and (equal? e1larg e2rarg) (equal? e1rarg e2larg)) )
       #t
       #f)]
      ;Operations are both either - or /
      [(and (equal? e1o e2o) (or (is-minus? e1o) (is-divide? e1o))) (if
       (and (equal? e1larg e2larg) (equal? e1rarg e2rarg))
       #t
       #f)]
      ;Operations are different
      [else #f])))

(define base-equal
  (lambda (e1 e2)
    ;assume that this is the base case
    (cond
      ;Operations are both either + or *
      [(and (equal? (second e1) (second e2)) (or (is-plus? (second e1)) (is-times? (second e1)))) (if
       (or (and (equal? (first e1) (first e2)) (equal? (third e1) (third e2)))
           (and (equal? (first e1) (third e2)) (equal? (third e1) (first e2))) )
       #t
       #f)]
      ;Operations are both either - or /
      [(and (equal? (second e1) (second e2)) (or (is-minus? (second e1)) (is-divide? (second e1)))) (if
       (and (equal? (first e1) (first e2)) (equal? (third e1) (third e2)))
       #t
       #f)]
      ;Operations are different
      [else #f])))

(define equal-commute
  (lambda (e1 e2)
    (cond
      [(not (list? (first e1))) (base-equal e1 e2)]
      [(or (is-plus? (second e1)) (is-times? (second e1)))
       (if (and (equal-commute (first e1) (first e2)) (equal-commute (third e1) (third e2)))
           #t
           (and (equal-commute (first e1) (third e2)) (equal-commute (third e1) (first e2))))]
      [else (and (equal-commute (first e1) (first e2)) (equal-commute (third e1) (third e2)))]
    )))