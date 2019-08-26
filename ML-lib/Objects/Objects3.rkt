#lang racket
(require "../ML-Helpers.rkt")
(require (prefix-in TY- "../TypeSystem/TypeSystem1.rkt"))
(require (except-in "ObjectsHelpers.rkt" #%method-val))
(require test-engine/racket-tests)
(provide
 #%class
 #%type-check
 #%testI-typed
 #%testE-typed
 #%typed-let
 #%typed-func
 #%struct
 #%reassign
 (except-out
  (unprefix-out TY- "../TypeSystem/TypeSystem1.rkt")
  TY-#%type-check
  TY-#%testI-typed
  TY-#%testE-typed
  TY-#%typed-let
  TY-#%typed-func
  TY-#%struct
  TY-#%reassign))

;; Defaults to calling parent's methods and fields
;; Objects of same type don't share state

(define-syntax-rule (raise-error string args ...)
  (raise-user-error (~a (format string args ...))))

(define-syntax-rule (subsymbol symbol args ...)
  (string->symbol (substring (symbol->string symbol) args ...)))

(define (same-type? t1 t2 instance-types)
  (define (this.same-type? t1 t2) (same-type? t1 t2 instance-types))
  (or
   (anything? t1) (anything? t2)
   (equal? t1 t2)
   (object-equal?nominal t1 t2 instance-types this.same-type?)
   (func-equal? t1 t2 this.same-type?)
   (class-equal? t1 t2 instance-types this.same-type?)
   ))

(define (same-type?no-convert t1 t2 instance-types)
  (define (this.same-type? t1 t2) (same-type?no-convert t1 t2 instance-types))
  (or
   (object-equal?no-convert instance-types t1 t2)
   (anything? t1) (anything? t2)
   (equal? t1 t2)
   (func-equal? t1 t2 this.same-type?)
   (same-class? t1 t2)
   ))


(define (full-type-check ast)
  (full-type-checker ast same-type? same-type?no-convert))

(define-syntax-rule (#%type-check code ...)
  (begin
    ;(printf "~s~n" '(code ...)) ;; this is just a line I uncomment when I want to print out the parsed result
    (full-type-check '(code ...))
    code ...))

(define-syntax-rule (#%testI-typed exp args)
  (TY-#%testI exp (#%type-check args)))

(define-syntax-rule (#%testE-typed exp args)
  (TY-#%testE exp (#%type-check args)))

(define (convert-object val id)
  (if (= (ml-object-id val) id)
      val
      (convert-object (ml-object-parent val) id))
  )
(define-syntax convert-field
  (syntax-rules ()
    [(convert (#%method-val ((argument arg_type) ...) return-type body ... last-body) type '#%method)
     (lambda (argument ...)
       (set! argument (convert argument arg_type)) ...
       (check-decrement-fuel)
       body ... (convert last-body return-type))]
    [(convert val type '#%field)
     (let ([the-val val])
       (cond
         [(ml-object? the-val) (convert-object the-val (type #T))]
         [else the-val]))]
    ))
(define-syntax-rule (convert val type)
  (let ([value val])
    (cond
      [(ml-object? value) (convert-object value (type #T))]
      [else value])))
(define (convert-value val1 val2)
  (cond
    [(ml-object? val1) (convert-object val1 (ml-object-id val2))]
    [else val1]))

(define-syntax-rule (#%typed-let (name type bind) body ...)
  (let ([name (convert bind type)]) body ...))

(define-syntax-rule (#%typed-func name ((arg type) ...) return-type body ... lastBody)
  (define (name arg ...)
    (set! arg (convert arg type)) ...
    (check-decrement-fuel) body ... (convert lastBody return-type)))

(define-syntax-rule (#%struct struct-name ((e-name type) ...))
  (begin
    (define symbols (list (quote e-name) ...))
    (define id (next-global-id))
    (define name (quote struct-name))
    
    (define (struct-name e-name ...) ;; the constructor
      (define vals (list (convert e-name type) ...)) ;; the values given to the 'struct-name' constructor
      (define h (make-hash (map cons symbols vals)))
      (define len (hash-count h))

      (define get-func (lambda (e) (hash-ref h e)))
      (define set-func (lambda (e val) (hash-set! h e val)))
      
      (ml-struct name get-func set-func vals id))))

(define-syntax-rule (#%reassign value new-bind)
  (begin
    (set! value (convert-value new-bind value))
    value))

(define-syntax-rule (#%class class-name parent-name (parent-class) (#%field/method name type value) ...)

  (begin
    (define id (next-global-id))
    (define (class-name . get-id)
      (cond
        [(empty? get-id)
         (check-decrement-fuel)
      
         (let-if
          (not (equal? parent-class void))
          ([parent-name (parent-class)])
          (define name (convert-field value type (quote #%field/method)))
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
          (begin (set! name (convert-value new-val name)) name)]
         ...
         [(and has-parent? ((ml-object-exists-func parent-name) field '#%field))
          ((ml-object-set-func parent-name) field new-val)]
         [else (raise-error "~a objects have no field ~a." (quote class-name) field)]
         )

       (reference-func field '#%field))

          (ml-object (quote class-name) reference-func set-func contains-func (if (defined? parent-name) parent-name '_) id))]
        [else id]
        ))))