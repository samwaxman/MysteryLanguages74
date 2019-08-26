#lang racket
(require "../ML-Helpers.rkt")
(require (prefix-in OB- "../Objects/Objects1.rkt"))
(require "../TypeSystem/TypeSystemHelpers.rkt")
(provide
 #%to-array
 #%array-access
 #%type-check
 #%testI-typed
 #%testE-typed
 #%array-set
 (except-out
  (unprefix-out OB- "../Objects/Objects1.rkt")
  OB-#%type-check
  OB-#%testI-typed
  OB-#%testE-typed))

(define (same-type?no-convert t1 t2 instance-types)
  (define (this.same-type? t1 t2) (same-type?no-convert t1 t2 instance-types))
  (or
   (object-equal?sub+super t1 t2 instance-types this.same-type?)
   (anything? t1) (anything? t2)
   (equal? t1 t2)
   (func-equal? t1 t2 this.same-type?)
   (class-equal? t1 t2 instance-types this.same-type?)
   (nominal-struct-type-equal? t1 t2)
   ))

(define (same-type? t1 t2 instance-types)
  (define (this.same-type? t1 t2) (same-type? t1 t2 instance-types))
  (or
   (anything? t1) (anything? t2)
   (equal? t1 t2)
   (object-equal? t1 t2 instance-types this.same-type?)
   (func-equal? t1 t2 this.same-type?) ;; It's possible these should be no convert
   (subclass? t1 t2 instance-types this.same-type?)
   (nominal-struct-type-equal? t1 t2)
   ))

(define (full-type-check ast)
  (full-type-checker ast same-type? same-type?no-convert))

(define-syntax-rule (#%type-check code ...)
  (begin
    (full-type-check '(code ...))
    code ...))

(define-syntax-rule (#%testI-typed exp args)
  (OB-#%testI exp (#%type-check args)))

(define-syntax-rule (#%testE-typed exp args)
  (OB-#%testE exp (#%type-check args)))

(define-syntax-rule (#%to-array _type args ...) (list->vector (list args ...)))

(define (#%array-access array index)
  (array-either array index (lambda () (vector-ref array index))))

(define (#%array-set array index new-val)
  (array-either array index (lambda () (vector-set! array index new-val) new-val)))

(define (array-either array index execution)
  (begin
    (when (not (vector? array))
      (raise-user-error
       (~a "Array access requires an array. Given " (~my-s array) ".")))
    (when (not (exact-integer? index))
      (raise-user-error
       (~a "Array indices can only be integers. Given " (~my-s index) ".")))
  (if (or (>= index (vector-length array))
          (< index 0))
      (raise-user-error "Array index out of bounds.")
      (execution))))