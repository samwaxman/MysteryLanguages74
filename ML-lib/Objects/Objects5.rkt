#lang racket
(require "../ML-Helpers.rkt")
(require (prefix-in TY- "../TypeSystem/TypeSystem1.rkt"))
(require "ObjectsHelpers.rkt")
(require test-engine/racket-tests)
(provide
 #%method-val
 #%class
 #%type-check
 #%testI-typed
 #%testE-typed
 (except-out
  (unprefix-out TY- "../TypeSystem/TypeSystem1.rkt")
  TY-#%type-check
  TY-#%testI-typed
  TY-#%testE-typed))

;; Defaults to calling parent field and child object
;; Objects of same class don't share state

(define (full-type-check ast)
  (full-type-checker ast default-same-type? default-same-type?no-convert
                     #:parent-field #T))

(define-syntax-rule (#%type-check code ...)
  (begin
    ;(printf "~s~n" '(code ...)) ;; this is just a line I uncomment when I want to print out the parsed result
    (full-type-check '(code ...))
    code ...))

(define-syntax-rule (#%testI-typed exp args)
  (TY-#%testI exp (#%type-check args)))

(define-syntax-rule (#%testE-typed exp args)
  (TY-#%testE exp (#%type-check args)))

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

       (if (equal? field/method '#%field)
           (cond
             [(and has-parent? ((ml-object-exists-func parent-name) var field/method))
              ((ml-object-get-func parent-name) var field/method)]
             [else
              (hash-ref refernce-hash var)])
           (cond
             [(or (this.contains-func var field/method) (not has-parent?))
              (hash-ref refernce-hash var)]
             [else ((ml-object-get-func parent-name) var field/method)])
           ))

     (define (set-func field new-val)

       (cond
         [(and has-parent? ((ml-object-exists-func parent-name) field '#%field))
          ((ml-object-set-func parent-name) field new-val)]
         [(and (equal? field (quote name)) (equal? (hash-ref field/method-lookup field) '#%field))
          (begin (set! name new-val) name)]
         ...)

       (reference-func field '#%field))

     (ml-object (quote class-name) reference-func set-func contains-func '_ (next-global-id)))
    ))