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

;This language will use the printed representation of a field
;name even if it isn't a string, just like Fields3. It also, as a prank,
;is case insensitive (it lowercases all field names)


(define-syntax-rule (field-cond field)
  (values #t field))
(define-syntax-rule (field-trans field)
  (string-downcase (~my-a field)))

;value-trans
(define-syntax-rule (iden-trans x)
  x)

;Can't happen - no values are dissallowed (condition is #t)
(define (on-create-bad-field) (void))
(define (on-access-bad-field) (void))

;return transform must be function, so not using iden-trans
;using the identity function
(define-syntax-rule (my-mod body ...)
  (parameterize-mod field-cond iden-trans identity field-trans
                    on-create-bad-field on-access-bad-field
                    body ...))