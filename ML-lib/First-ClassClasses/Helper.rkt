#lang racket
(require (prefix-in CO- "../Conditionals/Conditionals1.rkt"))
(require "../ML-Helpers.rkt"
         racket/format
         racket/stxparam
         (for-syntax syntax/parse)
         (except-in
          "../TypeSystem/TypeSystemHelpers.rkt"
          default-same-type? default-same-type?no-convert
          ))
(require test-engine/racket-tests)
(require racket/hash)
(provide
 (struct-out nominal-struct-type) (struct-out func-type) (struct-out struct-type) (struct-out class-type) (struct-out object-type)
 func-equal?
 raise-error
 next-global-id
 #%typed-func
 #%typed-let
 #%struct #%reassign
 #%reassign-field
 #%method-call
 new-full-type-checker
 default-same-type? default-same-type?no-convert nominal-struct-type-equal? same-class?
 anything? object-equal? constructor-class-equal? constructor-class-equal?sub func-equal? class-equal? constructor? subclass?
 class-fields+methods class-fields+methods-list get-instance-type)

#|
known bugs:
struct-get can only be called with 2 identifiers
|#

; A type is one of:
; | 'String
; | 'Number
; | 'Void
; | (struct-type (list (Symbol Type) ...) Number)
; | (func-type(list type ...) type)
; | (nominal-type Symbol Number)

(define-equality class-type? (same-class? class1 class2 instance-types this.same-type?)
  (subclass? class1 class2 instance-types this.same-type?)
  (subclass? class2 class1 instance-types this.same-type?))
  
(define (constructor? func)
  (and (func-type? func)
       (equal? (func-type-args func) '())
       (object-type? (func-type-return func))))

(define (is-class? type)
  (or (class-type? type) (constructor? type)))

(define (constructor-to-class constructor/class instance-types)
  (if (constructor? constructor/class)
      (get-instance-type instance-types (func-type-return constructor/class))
      constructor/class))

(define (constructor-class-equal? t1 t2 instance-types same-type-func?)
  (and
  ; (is-class? t1) (is-class? t2)
  ; (same-class? (constructor-to-class t1 instance-types) (constructor-to-class t2 instance-types) instance-types same-type-func?)))
   (constructor-class-equal?sub t1 t2 instance-types same-type-func?)
   (constructor-class-equal?sub t2 t1 instance-types same-type-func?)))
   
(define (constructor-class-equal?sub t1 t2 instance-types same-type-func?)
  (and
   (is-class? t1) (is-class? t2)
   (subclass? (constructor-to-class t1 instance-types) (constructor-to-class t2 instance-types) instance-types same-type-func?)))
    
(define (default-same-type? t1 t2 instance-types)
  (define (this.same-type? t1 t2) (default-same-type? t1 t2 instance-types))
  (or
   (anything? t1) (anything? t2)
   (equal? t1 t2)
   (object-equal? t1 t2 instance-types this.same-type?)
   (constructor-class-equal?sub t1 t2 instance-types this.same-type?)
   (func-equal? t1 t2 this.same-type?) ;; It's possible these should be no convert
   (subclass? t1 t2 instance-types this.same-type?)
   (nominal-struct-type-equal? t1 t2)
   ))

(define (default-same-type?no-convert t1 t2 instance-types)
  (define (this.same-type? t1 t2) (default-same-type?no-convert t1 t2 instance-types))
  (or
   (object-equal?sub+super t1 t2 instance-types this.same-type?)
   (anything? t1) (anything? t2)
   (equal? t1 t2)
   (constructor-class-equal? t1 t2 instance-types this.same-type?)
   (func-equal? t1 t2 this.same-type?)
   (class-equal? t1 t2 instance-types this.same-type?)
   (nominal-struct-type-equal? t1 t2)
   ))

(define (new-global-show-type instance-types type)
  (global-show-type instance-types type #:show-object-classes #T))

(define (new-full-type-checker ast same-type? same-type?no-convert)
  (full-type-checker ast same-type? same-type?no-convert #:show-type-func new-global-show-type))