#lang racket
(require (for-syntax syntax/parse))
(require "../ML-Helpers.rkt"
         (prefix-in F1- "../Fields/Fields1.rkt")
         "../Fields/FieldsHelpers.rkt")

(provide
 (except-out (unprefix-out F1- "../Fields/Fields1.rkt") F1-#%app)
 (rename-out [my-app #%app]))


;Copies records shallowly on application. Each field value
;is a copy, but if it was to an object (another record)
;that record is the same record as it was in the original.
(define (shallow-copy to-copy-rec)
  (let ([mapping (make-hash)])
    (for-each (lambda (field)
                (hash-set! mapping field
                           (hash-ref
                            (rec-mapping to-copy-rec)
                            field
                            (lambda () (error "Copy field not found!")))))
              (rec-order to-copy-rec))
    (rec mapping (rec-order to-copy-rec))))

(define (copy-if-rec obj)
      (if (rec? obj) (shallow-copy obj) obj))


(define-syntax-rule (my-app proc args ...)
    (F1-#%app proc (copy-if-rec args) ...))
