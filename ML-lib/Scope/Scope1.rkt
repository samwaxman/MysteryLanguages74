#lang racket
(require (prefix-in NF1- "../NamedFunctions/NamedFunctions1.rkt"))
(require "../ML-Helpers.rkt"
         syntax/macro-testing)
(provide
 (unprefix-out NF1- "../NamedFunctions/NamedFunctions1.rkt")
 #%reassign)

;This will throw an error that looks like
;set!: unbound identifier in module
;if id isn't bound, but we're catching that error
;with some other functions and turning it into
;Unbound identifier: id
(define-syntax-rule (#%reassign id new-value)
  (let ([eval-new-val new-value])
    (safe-set id eval-new-val)
    eval-new-val))



(define-syntax-rule (safe-set id new-val)
  (let ([prompt-tag (make-continuation-prompt-tag)])
  (call-with-continuation-prompt (lambda ()
                                   (call-with-exception-handler
                                    (lambda (x) (abort-current-continuation prompt-tag x))
                                    (lambda () (convert-compile-time-error (set! id new-val)))))
                                 prompt-tag (lambda (v) (raise v)))))

