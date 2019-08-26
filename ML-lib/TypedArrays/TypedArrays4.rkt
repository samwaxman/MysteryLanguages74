#lang racket
(require "../ML-Helpers.rkt")
(require (prefix-in OB- "../Objects/Objects1.rkt"))
(require "../TypeSystem/TypeSystemHelpers.rkt")
(provide
 #%to-array
 #%array-access
 #%array-set
 #%typed-func
 (except-out
  (unprefix-out OB- "../Objects/Objects1.rkt")
  OB-#%typed-func))

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

(define-syntax-rule (if-vector-copy arg)
  (cond
    [(vector? arg) (set! arg (vector-copy arg))]))

(define-syntax-rule (#%typed-func name ((arg type) ...) return-type body ... lastBody)
  (define (name arg ...)
    (if-vector-copy arg) ...
    (check-decrement-fuel) body ... lastBody))