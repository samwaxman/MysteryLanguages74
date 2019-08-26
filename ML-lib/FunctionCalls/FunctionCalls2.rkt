#lang racket
;THIS LANGUAGE IMPLEMENTS FUNCTION CALLS USING GOTO'S.
;IN PARTICULAR, ONCE A FUNCTION RETURNS, IT WILL RETURN
;FOR ITS CORRESPONDING BODY INSTEAD OF GOING BACK TO WHERE YOU
;WOULD NORMALLY THINK IT DOES. I.E (+ 1 (f x)) WILL JUST RETURN
;(f x) (or if f calls another function in its body, it will return
;that function's result instead)

(require (prefix-in Cond- "../Conditionals/Conditionals3.rkt")
         "../ML-Helpers.rkt"
         (prefix-in Scope- (only-in "../Scope/Scope1.rkt" #%reassign))
         (for-syntax syntax/parse))
(provide
 (rename-out [my-module #%module-begin]
             [Scope-#%reassign #%reassign])
 (except-out (unprefix-out Cond- "../Conditionals/Conditionals3.rkt")
             Cond-#%module-begin)
 #%func)

;We make a tag to tell our functions where to return to.
(define prompt-tag (make-continuation-prompt-tag))

;When they return, they throw away the current continuation and jump
;to our prompt tag.
(define-syntax-rule (#%func name (args ...) body ... last-body)
  (define (name args ...)
      (check-decrement-fuel)
      (let ([result ((lambda () body ... last-body))])
        (abort-current-continuation prompt-tag result))))



;Calls each body (save for function definitions)
;with a call/cc.
(define-syntax (my-module stx*)
  (syntax-case stx* ()
    [(_ body ...)
     #'(Cond-#%module-begin
        (expand body) ...)]))
;Evaluates a body with continuation handling
(define-syntax (continuation-evaluate stx)
  (syntax-parse stx
    [(_ body) #'(call-with-continuation-prompt
                      (lambda ()  (let ([res (let () (values) body)]) res))
                      prompt-tag
                      (lambda (v) v))]))


;Original mystery-language just had it so the first time a function returned
;the game was over. As we'd like to test these, and maybe even one day
;return multiple values, the behavior is slightly different. Every body gets evaluated
;with its own continuation-evaluate instead of wrapping it around the entire module.
;For bodies like let and block, the evaluate doesn't wrap them, but wraps their sub-parts.

;For the testers, it wraps the programs, as only makes sense.
(define-syntax (expand stx)
  (syntax-case stx ()
    [(_ body) (syntax-parse #'body #:literals(#%func Cond-#%let Cond-#%testI Cond-#%testE Cond-#%block)
                [(#%func _ ...) #'body]
                [(Cond-#%testI str prog) #'(Cond-#%testI str (expand prog))]
                [(Cond-#%testE str prog) #'(Cond-#%testE str (expand prog))]
                [(Cond-#%block block-body ...) #'(Cond-#%block (expand block-body) ...)]
                [(Cond-#%let ([id arg]) let-body ...) #'(Cond-#%let ([id (expand arg)])
                                                    (expand let-body) ...)]
                [x #'(continuation-evaluate x)])]))
