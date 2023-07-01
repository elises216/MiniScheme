#lang racket
;By Elise Schwarz-Saccamano and Jenny Rowlett
;We have adhered to the honor code in this assignment.

(provide env
         env?
         empty-env
         empty-env?
         env-syms
         env-vals
         env-previous
         env-lookup
         env-a
         env-b)
         ;top-level-env)

;The empty environment is null.
(define empty-env null)
(define empty-env? null?)

(struct env (syms vals previous) #:transparent) ;environemnt
;syms = list-of-symbols
;vals = list-of-values
;previous = previous-env

(define env-a
  (env '(x y) (map box'(1 2)) empty-env))
(define env-b
  (env '(x z) (map box '(5 7)) env-a))

(define (env-lookup environment symbol)
  (cond [(empty-env? environment) (error 'env-lookup "No binding for ~s" symbol)] ;if the environemnt is empty, return an error 
        [else (let* ([symlst (env-syms environment)] ;list of symbols 
                     [vallst (env-vals environment)]) ;list of values 
                (cond [(member symbol symlst) (list-ref vallst (index-of symlst symbol))] ;if symbol is in the symbol list, then get the value associated with that symbol 
                      [else (env-lookup (env-previous environment) symbol)]))])) ;else, look in the previous environment for the symbol



