#lang racket
(require "../ML-Helpers.rkt"
         (rename-in racket
                    [> r>]
                    [< r<]
                    [<= r<=]
                    [>= r>=]))
(provide != > < <= >= == ++)
;!= is not equals, == is equals, both of which
; only work on numbers. ++ is string append.
                                                 
(define (!= a b)
  (check-contract '!= (a b) (real? real?) ("number" "number"))
  (not (= a b)))
(define (> a b)
  (check-contract '> (a b) (real? real?) ("number" "number"))
  (r> a b))
(define (< a b)
  (check-contract '< (a b) (real? real?) ("number" "number"))
  (r< a b))
(define (<= a b)
  (check-contract '<= (a b) (real? real?) ("number" "number"))
  (r<= a b))
(define (>= a b)
  (check-contract '>= (a b) (real? real?) ("number" "number"))
  (r>= a b))
(define (== a b)
  (check-contract '== (a b) (real? real?) ("number" "number"))
  (= a b))
(define (++ a b)
  (check-contract '++ (a b) (string? string?) ("string" "string"))
  (string-append a b))