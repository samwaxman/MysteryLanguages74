#lang racket

;;BY REFERENCE (function paramaters variable alias)


(require (prefix-in Cond- "../Conditionals/Conditionals3.rkt")
         "../ML-Helpers.rkt"
         "MutableVarsHelpers.rkt"
         (for-syntax syntax/parse))
(provide
 (rename-out [my-app #%app] [boxed-recap recap] [boxed-print print])
 (except-out (unprefix-out Cond- "../Conditionals/Conditionals3.rkt")
             Cond-#%let Cond-#%app Cond-#%id Cond-recap Cond-print)
 #%let
 #%lambda
 #%reassign
 #%func
 #%id)


;Regular variable aliasing. If we're passing arguments to
;a user-defined function, give them boxes of the arguments instead
(define-syntax-rule (my-app proc arg ...)
  (let ([evaluated-proc proc])
    (if (function? evaluated-proc)
        (#%arity-app (function-lam evaluated-proc) (expand-arg arg) ...)
        (#%arity-app evaluated-proc arg ...))))


;boxing recap so (#%id ) will be nice to it
(define boxed-recap
  (box Cond-recap))
(define boxed-print
  (box student-print))
