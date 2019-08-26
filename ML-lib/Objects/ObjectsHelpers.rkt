#lang racket
(require "../ML-Helpers.rkt"
         "../TypeSystem/TypeSystemHelpers.rkt"
         racket/format
         racket/stxparam
         (for-syntax syntax/parse))
(provide
 mem?
 next-global-id
 #%method-val
 full-type-checker let-if defined?
 default-same-type? default-same-type?no-convert same-class?
 anything? object-equal?nominal func-equal? nominal-subclass?
 class-equal? object-equal?no-convert)

(define (mem? e l) (list? (memv e l)))

(define-syntax-rule (#%method-val ((argument type) ...) return-type body ...)
  (lambda (argument ...)
    (check-decrement-fuel)
    body ...))

(define-syntax-rule (let-if cond ([arg val] ...) body ...)
  (if cond
      (let ([arg val] ...) body ...)
      (let () body ...)))

(define-syntax (defined? stx)
  (syntax-case stx ()
    [(_ id)
     (with-syntax ([v (identifier-binding #'id)])
       #''v)]))