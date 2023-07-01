#lang racket
;By Elise Schwarz-Saccamano and Jenny Rowlett
;We have adhered to the honor code in this assignment.

(require "parse.rkt" "env.rkt")
(require rackunit)

(provide eval-exp
         init-env
         apply-proc
         apply-primitive-op
         primitive-operators
         prim-env
         prim-proc
         prim-proc?
         prim-proc-symbol
         closure
         closure?
         closure-params
         closure-body
         closure-env)

;Main Procedure 
(define (eval-exp tree e) ;evaluate tree in environment e 
  (cond [(lit-exp? tree) (lit-exp-num tree)] ;MiniScheme A
        [(var-exp? tree)(unbox (env-lookup e (var-exp-symbol tree)))] ;MiniScheme B
        [(app-exp? tree)
         (let ([proc (eval-exp (app-exp-proc tree) e)]
               [args (map (λ (arg)
                            (eval-exp arg e))
                          (app-exp-args tree))])
               (apply-proc proc args))] ;apply-proc
        [(ite-exp? tree) ;MiniScheme D
         (let ([op (eval-exp (ite-exp-cond tree) e)])
           (if (equal? op 'True)
               (eval-exp (ite-exp-then tree) e)
               (eval-exp (ite-exp-else tree) e)))]
        [(let-exp? tree) ;MiniScheme E 
         (let* ([binding-list (map (λ (exp)
                                     (eval-exp exp e))
                                   (let-exp-parsed tree))]
                [symbol-list (let-exp-symbols tree)]
                [new-env (env symbol-list (map box binding-list) e)] ;Changed to box Minischeme G
                [body (let-exp-body tree)])
           (eval-exp body new-env))]
       [(lambda-exp? tree) ;MiniScheme F
         (closure (lambda-exp-param tree) (lambda-exp-body tree) e)]
       [(set-exp? tree)
        (set-box! (env-lookup e (set-exp-symbol tree))(eval-exp (set-exp-expression tree) e))] ;Minischeme G
       [(begin-exp? tree)
        (let ([exps (begin-exp-exps tree)])
        (foldl (λ (exp acc) (eval-exp exp e)) (void) exps))] ;Minischeme G
        [else(error 'eval-exp "Invalid tree: ~s" tree)]))

;Primative Operator Environment
(struct prim-proc (symbol) #:transparent) ;a prim-proc is a value that will be returned by eval-exp

(define primitive-operators ;list of primitive operators 
  '(+ - * / add1 sub1 negate list cons car cdr eqv? lt? gt? leq? geq? null? list? number?))

(define (apply-proc proc args)
  (cond [(prim-proc? proc) ;checks if proc is a primitive procedure 
         (apply-primitive-op (prim-proc-symbol proc) args)] ;if it is, then apply the procdure to the arguments
        [(closure? proc) ;checks if proc is a closure
         (let ([new-env (env (closure-params proc) (map box args) (closure-env proc))]) ;Changed to box MiniSchemeG
           (eval-exp (closure-body proc) new-env))]
      ;  [(void? proc) args]
        [else (error 'apply-proc "bad procedure: ~s" proc)]))

(define (apply-primitive-op op args)
  (cond [(eq? op '+) (apply + args)]
        [(eq? op '-) (apply - args)]
        [(eq? op '*) (apply * args)]
        [(eq? op '/) (apply / args)]
        [(eq? op 'add1) (apply add1 args)]
        [(eq? op 'sub1) (apply sub1 args)]
        [(eq? op 'negate)
         (apply (λ (x) (* -1 x)) args)]
        [(eq? op 'list) (apply list args)]
        [(eq? op 'cons) (apply cons args)]
        [(eq? op 'car) (apply car args)]
        [(eq? op 'cdr) (apply cdr args)]
        [(eq? op 'eqv?)
         (if (equal? (apply = args) #t)
             'True
             'False)]
        [(eq? op 'lt?)
         (if (equal? (apply < args) #t)
             'True
             'False)]
        [(eq? op 'gt?)
         (if (equal? (apply > args) #t)
             'True
             'False)]
        [(eq? op 'leq?)
         (if (equal? (apply <= args) #t)
             'True
             'False)]
        [(eq? op 'geq?)
         (if (equal? (apply >= args) #t)
             'True
             'False)]
        [(eq? op 'number?)
         (if (equal? (apply number? args) #t)
             'True
             'False)]
        [(eq? op 'list?)
         (if (or (equal? (apply list? args) #t)(eq? (first args) 'null))
             'True
             'False)]
        [(eq? op 'null?)
         (if (or (null? (first args)) (eq? (first args) 'null))
             'True
             'False)]
        [else (error 'apply-primitive-op "Unknown primitive: ~s" op)]))

;Adding Primitives to Initial Environment
(define prim-env
  (env primitive-operators
       (map box (map prim-proc primitive-operators))
       empty-env))

(define init-env
  (env '(x y True False null)
       (map box'(23 42 'True 'False ()))
       prim-env))

;MiniScheme F (Closures)
(struct closure (params body env) #:transparent)