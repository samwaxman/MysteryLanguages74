#lang racket
(require "../ML-Helpers.rkt")
(require "Helper.rkt")
(require (prefix-in OB- "../Objects/Objects1.rkt"))
(require test-engine/racket-tests)
(provide
 #%type-check
 #%testI-typed
 #%testE-typed
 (except-out
  (unprefix-out OB- "../Objects/Objects1.rkt")
  OB-#%type-check
  OB-#%testI-typed
  OB-#%testE-typed))

(define (same-type? t1 t2 instance-types)
  (or
   (anything? t1) (anything? t2)
   (equal? t1 t2)
   (object-equal? t1 t2 instance-types (lambda (t1 t2) (same-type? t1 t2 instance-types)))
   (constructor-class-equal?sub t1 t2 instance-types (lambda (t1 t2) (default-same-type?no-convert t1 t2 instance-types)))
   (func-equal? t1 t2 (lambda (t1 t2) (same-type? t1 t2 instance-types))) ;; It's possible these should be no convert
   (subclass? t1 t2 instance-types (lambda (t1 t2) (default-same-type?no-convert t1 t2 instance-types)))
   (nominal-struct-type-equal? t1 t2)
   ))

(define (full-type-check ast)
  (new-full-type-checker ast same-type? default-same-type?no-convert))

(define-syntax-rule (#%type-check code ...)
  (begin
    (full-type-check '(code ...))
    code ...))

(define-syntax-rule (#%testI-typed exp args)
  (OB-#%testI exp (#%type-check args)))

(define-syntax-rule (#%testE-typed exp args)
  (OB-#%testE exp (#%type-check args)))