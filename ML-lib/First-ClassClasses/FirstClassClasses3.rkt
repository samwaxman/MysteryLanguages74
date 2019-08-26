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

(define (special-fields/methods-equal? fields/methods1 fields/methods2 same-type-func?)
  (andmap
   (lambda (fields/methods)
     (match fields/methods
       [(list (list name-const type-const) (list name type))
        (or (not (equal? name-const name)) (same-type-func? type-const type))]) ;; possibly need to flip around type-const and type in same-type-func?
     (cartesian-product fields/methods1 fields/methods2))))

(define (constructor-class-equal?special t1 t2 instance-types same-type-func?)
  (and
   (constructor? t1) (class-type? t2)
   (or
    (subclass? (get-instance-type instance-types (func-type-return t1)) t2 instance-types same-type-func?)
    (subclass? t2 (get-instance-type instance-types (func-type-return t1)) instance-types same-type-func?)
    )))

(define (same-type? t1 t2 instance-types)
  (define (this.same-type? t1 t2) (same-type? t1 t2 instance-types))
  (or
   (anything? t1) (anything? t2)
   (equal? t1 t2)
   (object-equal? t1 t2 instance-types this.same-type?)
   (constructor-class-equal?special t1 t2 instance-types this.same-type?)
   (func-equal? t1 t2 this.same-type?)
   (subclass? t1 t2 instance-types this.same-type?)
   (subclass? t2 t1 instance-types this.same-type?)
   (nominal-struct-type-equal? t1 t2)
   ))

(define (full-type-check ast)
  (new-full-type-checker ast same-type? default-same-type?no-convert
                    ))

(define-syntax-rule (#%type-check code ...)
  (begin
    (full-type-check '(code ...))
    code ...))

(define-syntax-rule (#%testI-typed exp args)
  (OB-#%testI exp (#%type-check args)))

(define-syntax-rule (#%testE-typed exp args)
  (OB-#%testE exp (#%type-check args)))