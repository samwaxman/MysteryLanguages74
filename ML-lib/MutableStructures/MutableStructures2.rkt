#lang racket
(require (for-syntax syntax/parse))
(require "../ML-Helpers.rkt"
         (prefix-in F1- "../Fields/Fields1.rkt")
         "../Fields/FieldsHelpers.rkt")
(require (prefix-in MV3- "../MutableVars/MutableVars3.rkt"))

(provide
 (except-out (unprefix-out F1- "../Fields/Fields1.rkt") F1-#%app)
 (rename-out [my-app #%app]))

(define copy-depth 1000)
(define (copy-if-rec obj)
  (if (rec? obj)
      (if (< copy-depth 1) (raise-user-error "Program timed out")
          (begin (set! copy-depth (- copy-depth 1)) (deep-copy obj))) obj))

;Deep copy of the record. A record passed into a function will be copied
;completely. The new record will have no shared references with the old and
;can be modified with impunity.
(define (deep-copy to-copy-rec)
  (let ([mapping (make-hash)])
    (for-each (lambda (field)
                (hash-set!
                 mapping field
                 (copy-if-rec
                  (hash-ref (rec-mapping to-copy-rec)
                            field
                            ;As we're going across order, this should NEVER happen.
                            (lambda () (error "Copy field not found!"))))))
              (rec-order to-copy-rec))
    (rec mapping (rec-order to-copy-rec))))


;;Test this with side effects.
(define-syntax-rule (my-app proc args ...)
  (F1-#%app proc (copy-if-rec args) ...))

