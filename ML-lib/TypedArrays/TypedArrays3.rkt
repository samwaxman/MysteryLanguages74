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

(define (full-type-check ast)
  (full-type-checker ast default-same-type? default-same-type?no-convert
                     #:array-subtyping #F))

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