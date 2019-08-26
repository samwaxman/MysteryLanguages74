#lang racket
(require "../ML-Helpers.rkt")
(require "TypeSystemHelpers.rkt")
(require (prefix-in CO- "../Conditionals/Conditionals1.rkt"))
(require test-engine/racket-tests)
(provide
 #%typed-let
 #%typed-func
 #%struct
 #%field-id
 #%type-check
 #%testI-typed
 #%testE-typed
 #%reassign #%reassign-field
 #%method-call
 (unprefix-out CO- "../Conditionals/Conditionals1.rkt"))

(define (full-type-check ast)
  (full-type-checker ast default-same-type? default-same-type?no-convert))

(define-syntax-rule (#%type-check code ...)
  (begin
    ;(printf "~s" 'code) ... (printf "~n") ;; this is just a line I uncomment when I want to print out the parsed result
    (full-type-check '(code ...))
    code ...))

(define-syntax-rule (#%testI-typed exp args)
  (CO-#%testI exp (#%type-check args)))

(define-syntax-rule (#%testE-typed exp args)
  (CO-#%testE exp (#%type-check args)))
