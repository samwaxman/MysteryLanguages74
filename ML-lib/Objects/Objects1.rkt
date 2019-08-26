#lang racket
(require "../ML-Helpers.rkt")
(require (prefix-in TY- "../TypeSystem/TypeSystem1.rkt"))
(require "ObjectsHelpers.rkt")
(require test-engine/racket-tests)
(provide
 #%method-val
 #%class
 (except-out
  (unprefix-out TY- "../TypeSystem/TypeSystem1.rkt")))

(define-syntax-rule (raise-error string args ...)
  (raise-user-error (~a (format string args ...))))

(define-syntax-rule (subsymbol symbol args ...)
  (string->symbol (substring (symbol->string symbol) args ...)))

;; Defaults to calling child fields and methods
;; Objects of same class don't share state

(define-syntax-rule (#%class class-name parent-name (parent-class) (#%field/method name _type value) ...)
    
  (define (class-name)
    (check-decrement-fuel)
    (let-if
     (not (equal? parent-class void))
     ([parent-name (parent-class)])
     (define name value)
     ...

     (define field/method-lookup
       (make-immutable-hash
        (list (cons (quote name) (quote #%field/method)) ...)))
    
     (define has-parent? (and (defined? parent-name) (ml-object? parent-name)))

     (define (this.contains-func var field/method)
      
       (and
        (hash-has-key? field/method-lookup var)
        (equal? (hash-ref field/method-lookup var) field/method)))

     (define (contains-func var field/method)
       (or
        (this.contains-func var field/method)
        (and has-parent? ((ml-object-exists-func parent-name) var field/method))))
    
     (define (reference-func var field/method)
      
       (define refernce-hash
         (make-immutable-hash
          (list (cons (quote name) name) ...)))

       (cond
         [(this.contains-func var field/method)
          (hash-ref refernce-hash var)]
         [(and has-parent? ((ml-object-exists-func parent-name) var field/method))
          ((ml-object-get-func parent-name) var field/method)]
         [else
          (raise-error "~a objects have no ~a ~a." (quote class-name) (subsymbol field/method 2) var)]))

     (define (set-func field new-val)

       (cond
         [(and (equal? field (quote name)) (equal? (hash-ref field/method-lookup field) '#%field))
          (begin (set! name new-val) name)]
         ...
         [(and has-parent? ((ml-object-exists-func parent-name) field '#%field))
          ((ml-object-set-func parent-name) field new-val)]
         [else (raise-error "~a objects have no field ~a." (quote class-name) field)]
         )

       (reference-func field '#%field))

     (ml-object (quote class-name) reference-func set-func contains-func '_ (next-global-id)))
    ))