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

;This language requires that field names are strings
;but they can be things that evaluate to strings, not just
;literal strings



(define-syntax-rule (field-cond field)
  (values (string? field) field))

;field and value
(define-syntax-rule (iden-trans x)
  x)



(define (on-create-bad-field field)
  (raise-user-error (string-append "Attempted to construct a record with a field that is not a string: " (~my-s field))))
(define (on-access-bad-field field)
  (raise-user-error (string-append "Attempted to access a non-field on a record: " (~my-s field))))

;return transform must be function, so not using iden-trans
;using the identity function
(define-syntax-rule (my-mod body ...)
 (parameterize-mod field-cond iden-trans identity iden-trans
                   on-create-bad-field on-access-bad-field
                   body ...))