#lang racket

(require (prefix-in Cond- "../Conditionals/Conditionals3.rkt")
         "../ML-Helpers.rkt"
         "MutableVarsHelpers.rkt"
         (for-syntax syntax/parse))
(provide
 (except-out (unprefix-out Cond- "../Conditionals/Conditionals3.rkt")
             Cond-#%let
             Cond-#%app
             Cond-#%id
             Cond-recap
             Cond-print)
 (rename-out [my-app #%app]
             [boxed-recap recap]
             [boxed-print print])
 #%let
 #%lambda
 #%func
 #%reassign
 #%id)


;Call by copy restore. Instead of passing the actual reference to a value,
;we copy its reference and pass that in. Then, at the end of function evaluation,
;we take the original reference and update the value its pointing to to be the value
;of the copied reference is pointing to
(define-syntax (my-app stx)
  (syntax-parse stx
    [(_ proc args ...)
     (with-syntax ([(boxed-args ...) (generate-temporaries #'(args ...))]
                   [(inner-args ...) (generate-temporaries #'(args ...))])
       #'(let ([evaluated-proc proc])
           (if (function? evaluated-proc)
               (let* ([boxed-args (expand-arg args)] ...
                      [inner-args (box (unbox boxed-args))] ...
                      [result (#%arity-app (function-lam evaluated-proc) inner-args ...)])
                 (set-box! boxed-args (unbox inner-args)) ...
                 result)
               (#%arity-app evaluated-proc args ...))))]))

;boxing recap so (#%id ) will be nice to it
(define boxed-recap
  (box Cond-recap))
(define boxed-print
  (box student-print))
