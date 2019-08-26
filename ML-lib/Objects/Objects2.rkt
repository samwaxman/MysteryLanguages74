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

;; Defaults to calling fields and methods of child class
;; Objects of same class share state

(define-syntax-rule (#%class class-name parent-name (parent-class) (#%field/method name _type value) ...)

  (begin
      
    (define (make-class)
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
           [(or (this.contains-func var field/method) (not has-parent?))
            (hash-ref refernce-hash var)]
           [else ((ml-object-get-func parent-name) var field/method)]))

       (define (set-func field new-val)

         (cond
           [(and (equal? field (quote name)) (equal? (hash-ref field/method-lookup field) '#%field))
            (begin (set! name new-val) name)]
           ...
           [has-parent?
            ((ml-object-set-func parent-name) field new-val)])

         (reference-func field '#%field))

       (lambda () (ml-object (quote class-name) reference-func set-func contains-func '_ (next-global-id)))
       ))
    (define class-name (make-class))))