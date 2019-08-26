#lang racket
(require "../ML-Helpers.rkt")
(require (prefix-in MV- "../MutableVars/MutableVars1.rkt"))
(require "FieldsHelpers.rkt")
(require racket/stxparam)
(provide
 #%record
 #%reassign-field
 #%access
 (except-out (unprefix-out MV- "../MutableVars/MutableVars1.rkt") MV-#%module-begin)
 (rename-out [my-mod #%module-begin]
             [student-print print]))


;The difference between all languages is:
;which values can be a field name
;what you do to the field name a user types before storing it in the computer
;what you do to the field value before storing it
;what you do to a field value when you're returning it to the user
;The error messages it has for bad field creation or access

;These are field-cond, field-trans value-trans, return-trans,
;and on-create-bad-field/on-access-bad-field respectively.

;This language is like 1 in that it require literal string names,
;But it's also lazy, so values get stored in lambdas, and get evaluated
;on return


(define-syntax-rule (field-cond field)
  (values
  (string? (syntax->datum #'field))
  field))
(define (return-trans x)
  (x))

(define-syntax-rule (value-trans x)
  (lambda () x))
;field trans
(define-syntax-rule (iden-trans x)
  x)

(define (on-create-bad-field _) (raise-user-error "Record fields must be strings"))
(define (on-access-bad-field _) (raise-user-error "Field access must use strings"))

;return transform must be function, so not using iden-trans
;using the identity function
(define-syntax-rule (my-mod body ...)
 (parameterize-mod field-cond value-trans return-trans iden-trans
                   on-create-bad-field on-access-bad-field
                   body ...))