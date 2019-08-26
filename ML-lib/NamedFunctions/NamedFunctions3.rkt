#lang racket
(require (prefix-in Cond- "../Conditionals/Conditionals3.rkt")
         "../ML-Helpers.rkt")
(provide
 (unprefix-out Cond- "../Conditionals/Conditionals3.rkt")
 (rename-out [func #%func]))

;Allows users to put in more or fewer arguments to a function application
;than the function was defined to take in. If the user enters fewer,
;the ones they didn't put in are bound to 'Undefined.
(define-syntax-rule (func name (args ...) body ... last-body)
  (define name
    (let ([arity (length (list 'args ...))])
      (let ([proper-func (lambda (args ...) body ... last-body)])
        (procedure-rename
         (lambda user-args
           (check-decrement-fuel)
           (let ([arg-num (length user-args)])
             (if (>= arg-num arity)
                 (apply proper-func (take user-args arity))                        
                 (apply proper-func (append user-args (make-list (- arity arg-num) 'Undefined))))))
         'name)))))



