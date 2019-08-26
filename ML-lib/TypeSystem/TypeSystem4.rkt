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
 (unprefix-out CO- "../Conditionals/Conditionals1.rkt"))

(define (same-type? t1 t2 instance-types)

  (define (this.same-type? t1 t2) (same-type? t1 t2 instance-types))
  
  (cond
    [(and (nominal-struct-type? t1) (nominal-struct-type? t2))
     (and (this.same-type? (get-instance-type instance-types t1) (get-instance-type instance-types t2))
          (equal? (nominal-struct-type-name t1) (nominal-struct-type-name t2)))]
    
    [(and (struct-type? t1) (struct-type? t2))
     (equal? (struct-type-args t1) (struct-type-args t2))]
    
    [else (or (equal? t1 t2) (anything? t1) (anything? t2) (nominal-struct-type-equal? t1 t2) (func-equal? t1 t2 this.same-type?))]))

(define (full-type-check ast)
  (full-type-checker ast same-type? same-type?))

(define-syntax-rule (#%type-check code ...)
  (begin
    (full-type-check '(code ...))
    code ...))

(define-syntax-rule (#%testI-typed exp args)
  (CO-#%testI exp (#%type-check args)))

(define-syntax-rule (#%testE-typed exp args)
  (CO-#%testE exp (#%type-check args)))