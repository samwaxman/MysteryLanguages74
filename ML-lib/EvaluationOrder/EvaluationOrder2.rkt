#lang racket
(require (prefix-in F1- "../Fields/Fields1.rkt")
         "../ML-Helpers.rkt")
(require "EvaluationOrderSetup.rkt")
(provide
 (except-out (unprefix-out F1- "../Fields/Fields1.rkt")
             F1-#%let F1-#%lambda F1-#%app F1-#%func F1-#%reassign
             F1-#%id F1-#%module-begin F1-recap F1-print)
 (rename-out
  [my-app #%app]
  [my-mod #%module-begin]
  [my-wrapped-print print]
  [my-wrapped-recap recap])
 #%id
 #%func
 #%let
 #%lambda)


;Achieves laziness via wrapping things in lambdas
(define-syntax-rule (make-lazy expr)
  (wrap (lambda () expr)))
;and evaluates by evaluating the anon func
(define (evaluate-lazy-expr expr)
  (expr))

;Wraps the provided functions so they work like everything else
(define my-wrapped-print (make-lazy student-print))
(define my-wrapped-recap (make-lazy F1-recap))

(define-syntax-rule (my-mod body ...)
  (parameterized-mod-begin #:wrap make-lazy #:unwrap evaluate-lazy-expr
                           body ...))
