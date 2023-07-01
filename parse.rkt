#lang racket
;By Elise Schwarz-Saccamano and Jenny Rowlett
;We have adhered to the honor code in this assignment.

(provide parse
         lit-exp ; constructor
         lit-exp? ; recognizer
         lit-exp-num ; accessor
         var-exp
         var-exp?
         var-exp-symbol
         app-exp
         app-exp?
         app-exp-proc
         app-exp-args
         ite-exp
         ite-exp?
         ite-exp-cond
         ite-exp-then
         ite-exp-else
         let-exp
         let-exp?
         let-exp-symbols
         let-exp-parsed
         let-exp-body
         lambda-exp
         lambda-exp?
         lambda-exp-param
         lambda-exp-body
         set-exp
         set-exp?
         set-exp-symbol
         set-exp-expression
         begin-exp
         begin-exp?
         begin-exp-exps)
        
(require rackunit)

;Overall Parse Procedure 
(define (parse input)
  (letrec ([parse-error (λ () (error 'parse "Invalid syntax ~x" input))]) ;Error 
    (cond [(number? input) (lit-exp input)] ;MiniScheme A
          [(symbol? input) (var-exp input)] ;MiniScheme B
          [(list? input)   
           (cond [(equal? (first input) 'if)
                  (if (= (length input) 4)
                      (ite-exp (parse (second input))(parse (third input))(parse (fourth input)))
                      (parse error))] ;MiniScheme D
                 [(or (equal? (first input) 'let)(equal? (first input) 'let*))
                  (let-exp (map first (second input)) (map parse (map second (second input))) (parse (third input)))] ;MiniScheme E
                 [(equal? (first input) 'lambda)
                  (lambda-exp (second input)(parse (third input)))] ;MiniScheme F
                 [(equal? (first input) 'set!) 
                  (set-exp (second input) (parse (third input)))] ;MiniScheme G
                 [(equal? (first input) 'begin)
                  (begin-exp (map parse (rest input)))] ;MiniScheme G 
                 [(equal? (first input) 'letrec)
                  (let* ([syms (map first (second input))]
                        [exps (map second (second input))]
                        [body (third input)]
                        [outer-parsed (map (λ (s) (lit-exp 0)) syms)]
                        [new-syms (map (λ (s) (gensym)) syms)]
                        [inner-parsed (map parse exps)]
                        [begin (foldr (λ (s new-s acc)(cons (set-exp s (var-exp new-s)) acc))(list (parse body)) syms new-syms)]
                        [inner-let-exp (let-exp new-syms inner-parsed (begin-exp begin))])
                    (let-exp syms outer-parsed inner-let-exp))];MiniScheme H
                 [else (app-exp (parse (first input))(map parse (rest input)))])] ;MiniScheme C
          [else (parse-error)])))

;MiniScheme A (Numbers)
(struct lit-exp (num) #:transparent)

;MiniScheme B (Symbols)
(struct var-exp (symbol) #:transparent)

;Mini Scheme C (Primitive Procedures) 
(struct app-exp (proc args) #:transparent) ;an app-exp stores the parse tree for a procedure and a list of parse trees for the arguments

;Mini Scheme D (Conditionals)
(struct ite-exp (cond then else) #:transparent)

;Mini Scheme E (Let Expressions)
(struct let-exp (symbols parsed body) #:transparent)

;Symbols = A list of the symbols that are bound in the binding list
;Parsed = A list of the parsed expressions (i.e., trees) that the symbols are bound to
;Body = The let body

;Mini Scheme F (Lambda Expressions)
(struct lambda-exp (param body) #:transparent)

;Mini Scheme G (set! and begin)
(struct set-exp (symbol expression) #:transparent)
(struct begin-exp (exps) #:transparent)       


  
