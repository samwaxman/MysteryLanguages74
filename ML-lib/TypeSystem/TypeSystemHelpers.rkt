#lang racket
(require (prefix-in CO- "../Conditionals/Conditionals1.rkt"))
(require "../ML-Helpers.rkt"
         racket/format
         racket/stxparam
         (for-syntax syntax/parse))
(require test-engine/racket-tests)
(require racket/hash)
(provide
 (struct-out nominal-struct-type) (struct-out func-type) (struct-out struct-type) (struct-out class-type) (struct-out object-type)
 func-equal?
 raise-error
 next-global-id
 #%typed-func
 #%typed-let
 #%struct #%field-id
 #%reassign #%reassign-field
 #%method-call
 full-type-checker
 default-same-type? default-same-type?no-convert
 anything? object-equal?nominal class-equal? same-class? array-equal? func-equal? nominal-subclass? subclass? object-equal? object-equal?sub+super object-equal?no-convert
 class-fields+methods class-fields+methods-list nominal-struct-type-equal?
 global-show-type define-equality
 get-instance-type
 )

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

(struct struct-type (args) #:transparent)
(struct func-type (name args return checker) #:transparent)
(struct nominal-struct-type (name id) #:transparent)
(struct class-type (fields methods parent id)
  #:transparent)
(struct object-type (name id) #:transparent)
(struct array-type (type) #:transparent)

(define global-id -1)
(define (next-global-id)
  (increment global-id)
  global-id)

(define (get-instance-type instance-types type)
  (match type
    [(nominal-struct-type name id) (hash-ref instance-types id)]
    [(object-type name id) (hash-ref instance-types id)]
    [else (raise-error "get-instance-type given ~s." type)]))

(define (class-fields+methods class instance-types)
  (match class
    [(class-type fields methods (? object-type? parent) _id)
     (define-values (parent-fields parent-methods) (class-fields+methods (get-instance-type instance-types parent) instance-types))
     (values
      (remove-duplicates (append fields parent-fields))
      (remove-duplicates (append methods parent-methods)))]
    [(class-type fields methods _parent _id) (values fields methods)]))

(define (class-fields+methods-list class instance-types)
  (define-values (fields methods) (class-fields+methods class instance-types))
  (sort (append fields methods)
        #:key (lambda (val) (~s val)) string<?))

(define (contains? l e #:equality-check [equality-check? equal?])
  (ormap (lambda (x) (equality-check? e x)) l))

(define (subset? l1 l2 #:equality-check [equality-check? equal?])
  (andmap (lambda (e) (contains? l2 e #:equality-check equality-check?)) l1))

(define im-hash make-immutable-hash)

(define-syntax-rule (andmap+length func l ...)
  (and
   (= (length l) ...)
   (andmap func l ...)))

(define (not-parent? parent)
  (or (procedure? parent) (string? parent)))
(define (has-parent? class)
  (nor (procedure? (class-type-parent class)) (string? (class-type-parent class))))

(define-syntax-rule (define-equality if-type (name type1 type2 args ...) condition ...)
  (define (name type1 type2 args ...)
    (and
     (if-type type1) (if-type type2)
     condition ...)))
     
(define-equality func-type? (func-equal? f1 f2 this.same-type?)
  (andmap+length this.same-type? (func-type-args f2) (func-type-args f1)) ;; arguments flipped around b/c co-variants vs. contravariants (probably spelled wrong)
  (this.same-type? (func-type-return f1) (func-type-return f2)))

(define-equality class-type? (same-class? class1 class2)
  (= (class-type-id class1) (class-type-id class2)))

(define (nominal-subclass? class1 class2 instance-types)
  (or
   [same-class? class1 class2]
   (and
    [has-parent? class1]
    [nominal-subclass?
     (get-instance-type instance-types (class-type-parent class1))
     class2 instance-types])))

(define-syntax-rule (print+return string args ...)
  (begin
    (printf string args ...)
    (format string args ...)))

;; Is class 1 a subclass of class2?
(define-equality class-type? (subclass? class1 class2 instance-types this.same-type?)
  (subset? (class-fields+methods-list class2 instance-types) (class-fields+methods-list class1 instance-types)
           #:equality-check
           (lambda (t2 t1)
             (and (equal? (arg-name t1) (arg-name t2)) (this.same-type? (arg-type t1) (arg-type t2))))))
;)

(define-equality object-type? (object-equal? object1 object2 instance-types this.same-type?)
  (subclass? (get-instance-type instance-types object1) (get-instance-type instance-types object2) instance-types this.same-type?))

(define-equality object-type? (object-equal?nominal object1 object2 instance-types this.same-type?)
  (nominal-subclass? (get-instance-type instance-types object1) (get-instance-type instance-types object2) instance-types))

(define-equality object-type? (object-equal?no-convert object1 object2 instance-types)
  (same-class? (get-instance-type instance-types object1) (get-instance-type instance-types object2)))

(define-equality class-type? (class-equal? class1 class2 instance-types this.same-type?)
  (andmap+length this.same-type? (class-fields+methods-list class1 instance-types) (class-fields+methods-list class2 instance-types)))

(define-equality object-type? (object-equal?sub+super object1 object2 instance-types this.same-type?)
  (class-equal? (get-instance-type instance-types object1) (get-instance-type instance-types object2) instance-types this.same-type?))

(define-equality array-type? (array-equal? array1 array2 this.same-type?)
  (this.same-type? (array-type-type array1) (array-type-type array2)))

(define-equality nominal-struct-type? (nominal-struct-type-equal? type1 type2)
  (= (nominal-struct-type-id type1) (nominal-struct-type-id type2)))

(define (anything? type) (equal? type '(_)))
    
(define (default-same-type? t1 t2 instance-types)
  (define (this.same-type t1 t2) (default-same-type? t1 t2 instance-types))
  (or
   (anything? t1) (anything? t2)
   (equal? t1 t2)
   (array-equal? t1 t2 (lambda (t1 t2) (default-same-type?no-convert t1 t2 instance-types)))
   (object-equal? t1 t2 instance-types this.same-type)
   (func-equal? t1 t2 this.same-type)
   (subclass? t1 t2 instance-types this.same-type)
   (nominal-struct-type-equal? t1 t2)
   ))

(define (default-same-type?no-convert t1 t2 instance-types)
  (define (this.same-type t1 t2) (default-same-type?no-convert t1 t2 instance-types))
  (or
   (object-equal?sub+super t1 t2 instance-types this.same-type)
   (anything? t1) (anything? t2)
   (equal? t1 t2)
   (array-equal? t1 t2 this.same-type)
   (func-equal? t1 t2 this.same-type)
   (class-equal? t1 t2 instance-types this.same-type)
   (nominal-struct-type-equal? t1 t2)
   ))

(define (default-make-type class/struct-ids instance-types)
  (lambda (t)
    (define make-type (default-make-type class/struct-ids instance-types))
    (match t
      
      [(? (lambda (type) (hash-has-key? class/struct-ids type)) t)
       
       (match (hash-ref class/struct-ids t)
         [(cons id 'struct)
          (nominal-struct-type t id)]
         [(cons id 'class)
          (object-type t id)])]

      [(list '#%array-type (app make-type type)) (array-type type)]

      [(list '#%method-type name arguments return-type)
       (define (get-type arg) (if (pair? arg) (arg-type arg) arg))
       (define argument-types (map make-type (map get-type arguments)))
       (func-type name argument-types (make-type return-type) void)]

      [(list '#%field-type name type)
       (make-type type)]

      [(list '#%class-type id fields/methods ...)
       (define (convert val) (check-super (list (cadr val) (make-type val))))
       (define (check-super field/method)
         (if (equal? (arg-name field/method) 'super)
             (raise-error ERROR-STRING:super-field/method-name) field/method))
       (define fields-list (filter (lambda (x) (equal? (car x) '#%field-types)) fields/methods))
       (define methods-list (filter (lambda (x) (equal? (car x) '#%method-types)) fields/methods))
       (when (not (empty? fields-list)) (set! fields-list (cdar fields-list)))
       (when (not (empty? methods-list)) (set! methods-list (cdar methods-list)))
     
       (define fields (map convert (map (lambda (list) (cons '#%field-type list)) fields-list)))
       (define methods (map convert (map (lambda (l) (cons '#%method-type l)) methods-list)))

       (define names (append (map arg-name fields) (map arg-name methods)))
       (define duplicate-names (check-duplicates names))
       
       (cond
         [(not (boolean? duplicate-names)) (raise-error ERROR-STRING:duplicate-field/method-type-names duplicate-names)])

       (hash-set! instance-types id (class-type fields methods void id))
       (class-type fields methods void id)
       ]
       
      [(list name type)
       (list name (make-type type))]
      
      [(? (lambda (type) (member? type valid-types)) t) t]

      [t (raise-error ERROR-STRING:unbound-type-id t)
       ])))

(define (arg-name arg) (first arg))
(define (arg-type arg) (second arg))

(define (arg->pair arg)
  (cons (arg-name arg) (arg-type arg)))

(define (is-field? field/method)
  (equal? (car field/method) '#%field))
(define (is-method? field/method)
  (equal? (car field/method) '#%method))

(define (class-all class hash-set-func field/method instance-types)
  (define get-func (if (equal? field/method 'fields) class-type-fields class-type-methods))
  
  (define current (map arg->pair (get-func class)))

    (if (has-parent? class)
      (hash-set-func
       (class-all (get-instance-type instance-types (class-type-parent class)) hash-set-func field/method instance-types)
       current)
      (im-hash current)))

(define (struct-all-fields str)
  (im-hash (map arg->pair (struct-type-args str))))

(define built-in (make-immutable-hash
                  (list
                   (cons 'recap (cons (next-global-id) (func-type 'recap '() 'Void void)))
                   (cons 'print (cons (next-global-id) (func-type 'print '((_)) 'String void)))
                   )))

(define-syntax-rule (check-arg-number f item args)
  (when (not (= (length item) (+ args 1)))
    (raise-error ERROR-STRING:wrong-number-of-arguments f args (- (length item) 1))))

(define (is-boolean? item)
  (or (equal? item 'true) (equal? item 'false)))

(define (is-testI? item)
  (and (list? item) (equal? (car item) '#%testI-typed)))

(define (is-testE? item)
  (and (list? item) (equal? (car item) '#%testE-typed)))

(define (is-operator? item)
  (member? item (list '#%+ '#%- '#%* '#%/ '#%== '#%<= '#%>= '#%< '#%> '#%!=)))

(define (combine-hash h1 h2)
  (hash-union h1 h2 #:combine (lambda (a b)
                                (if (> (car a) (car b))  ;; If there is an error with function scoping it is likely from this line not being well thought out
                                    a b))))

(define (hash-filter hash pred)
  (make-immutable-hash (filter pred (hash->list hash))))

(define (combine-same h1 h2)
  (hash-filter h1 (lambda (pair) (hash-has-key? h2 (car pair)))))

(define (only-defined env-hash defined-hash)
  (hash-filter
   env-hash
   (lambda (pair)
     (match pair
       [(cons name (cons id value))
        (hash-has-key? defined-hash (cons name id))]))))
      
(define (global-show-type instance-types type  #:show-object-classes [show-object-classes #F])
  (global-show-type-with-depth instance-types type 1  #:show-object-classes show-object-classes))

; TODO: If a type is printed, it should be printed in the syntax used in the language!
(define (global-show-type-with-depth instance-types type depth #:show-object-classes [show-object-classes #F])
  (define (this.global-show-type type) (global-show-type-with-depth instance-types type (+ depth 1) #:show-object-classes show-object-classes))
  (define (fields-to-string fields)
    (if (empty? fields)
        ""
      (format "[Vars: ~a]" (truncate (comma-join fields (lambda (field) (~a (car field) " :: " (this.global-show-type (cadr field)))))))))
  (define (methods-to-string methods)
    (if (empty? methods)
        ""
       (format "[Funs: ~a]" (truncate (comma-join methods (lambda (func-t) (~a (car func-t) (this.global-show-type (cadr func-t)))))))))
  (define (combine-field-and-method-strings fields-string methods-string)
    (~a fields-string (if (and (not (equal? fields-string "")) (not (equal? methods-string ""))) " " "") methods-string))
  (define (comma-join list element-to-string)
    (if (empty? list)
        ""
        (substring (foldl (lambda (el str) (string-append str ", " (element-to-string el))) "" list) 2)))
  (define-syntax-rule (truncate expr)
    (if (> depth 3)
        "..."
        ((lambda () expr))))
  
  (match type
    [(? string? type) type]
    [(? symbol? type)
     (symbol->string type)]
    [(? list? type)
     (map (lambda (type) (this.global-show-type type)) type)]
    [(struct-type args)
     (format "[struct: ~a]" (truncate (this.global-show-type args)))]
    [(class-type fields methods parent _id)
     (let-values ([(fields methods) (class-fields+methods type instance-types)])
     (format "[Class: ~a]" (combine-field-and-method-strings (fields-to-string fields) (methods-to-string methods))))]
    [(func-type name (list) (? object-type? return) checker) ;; should I be doing this?
     (if show-object-classes
         (this.global-show-type (get-instance-type instance-types return))
         (format "() -> ~a" (this.global-show-type return)))]
    [(func-type name args return checker)
     (format "(~a) -> ~a" (comma-join (this.global-show-type args) ~a) (this.global-show-type return))]
    [(nominal-struct-type name id)
     name]
    [(object-type name id)
     (if show-object-classes
         (let-values([(fields methods) (class-fields+methods (get-instance-type instance-types type) instance-types)])
         (format "[Object: ~a]" (truncate (combine-field-and-method-strings (fields-to-string fields) (methods-to-string methods)))))
         name)]
    [(array-type type)
     (format "[array :: ~a]" (truncate (this.global-show-type type)))]
    ))

(define valid-types (list 'Number 'String 'Boolean void))

(define (member? v l) (list? (member v l)))


;; ERROR MESSAGES ;;
(define ERROR-STRING:wrong-number-of-arguments "Function ~a expects ~a arguments, but was called with ~a.")
(define ERROR-STRING:wrong-argument-type "Function ~a expects a ~a for argument ~a, but was given a ~a.")
(define ERROR-STRING:unbound-type-id "The type ~a is not defined.")
(define ERROR-STRING:then-else-mismatch "The 'then' and 'else' clauses of an 'if' must have the same type, but they were ~a and ~a respectively.")
(define ERROR-STRING:wrong-function-return-type "Function ~a expects to return a ~a, but instead returns a ~a.")
(define ERROR-STRING:wrong-decleration-type "The identifier ~a was declared to be of type ~a, but actually has type ~a.")
(define ERROR-STRING:not-a-struct-or-object "Tried to access field ~a of a ~a, but that is not a struct or object.")
(define ERROR-STRING:unbound-field "~a has no field named ~a.")
(define ERROR-STRING:unbound-method "~a has no method named ~a.")
(define ERROR-STRING:unbound-id "The identifier ~a is not defined.")
(define ERROR-STRING:not-a-function "Tried to call a value of type ~a, but that is not a function.")
(define ERROR-STRING:non-boolean-condition "The condition of an 'if' must be a boolean, but instead was a ~a.")
(define ERROR-STRING:wrong-reassignment-type "The variable ~a was reassigned to a value of type ~a instead of a value of type ~a.")
(define ERROR-STRING:non-existant-parent "Class ~a tried to extend ~a, but that is not defined.")
(define ERROR-STRING:non-class-parent "Class ~a tried to extend ~a, but that is not a class.")
(define ERROR-STRING:parent-child-field-type-mismatch "The field ~a has type ~a which is incompatable with the parent's ~a field which has type ~a.")
(define ERROR-STRING:parent-child-method-type-mismatch "The method ~a has type ~a which is incompatable with the parent's ~a field which has type ~a.")
(define ERROR-STRING:wrong-array-element-type "Tried to give a ~a array a ~a for element number ~a.")
(define ERROR-STRING:not-an-array "Tried to reference a value from ~a, but that is not an array.")
(define ERROR-STRING:wrong-array-element-set-type "Tried to set a value from a ~a array to a ~a.")
(define ERROR-STRING:not-an-array-set "Tried to set a value from ~a, but that is not an array.")
(define ERROR-STRING:expected-method-given-field "Fields are incompatible with methods, and ~a was already assigned to a method and therefore can't be assigned to a field.")
(define ERROR-STRING:expected-field-given-method "Methods are incompatible with fields, and ~a was already assigned to a field and therefore can't be assigned to a method.")
(define ERROR-STRING:duplicate-field/method-type-names "Can't declare a type with multiple fields/methods of the same name, but ~a is repeated.")
(define ERROR-STRING:super-field/method-name "Fields and methods can't be declared with the name super.")
;; ERROR MESSAGES ;;

(define (add-ids ast)
  (match ast ;; It's possible we need to add ids to arguments and let binds here as well
    
    [(list '#%struct name (list args ...))
     (list '#%struct (next-global-id) name (add-ids args))]

    [(list '#%typed-func name (list args ...) return-type body ...)
     (append
      (list '#%typed-func (next-global-id) name (add-ids args) (add-ids return-type)) (add-ids body))]

    [(list '#%method-val name (list args ...) return-type body ...)
     (append
      (list '#%method-val name (add-ids args) (add-ids return-type)) (add-ids body))]

    [(list '#%class name parent-name (list parent) field-bind ...)
     (append
      (list '#%class (next-global-id) name parent-name parent) (add-ids field-bind))] ;; IDs not added because they must only be added when the class is instantiated

    [(list '#%field name type bind)
     (list '#%field (next-global-id) name (add-ids type) (add-ids bind))]
    
    [(list '#%method name type bind)
     (list '#%method (next-global-id) name (add-ids type) (add-ids bind))]

    [(list '#%class-type fields/methods ...)
     (append (list '#%class-type (next-global-id)) (add-ids fields/methods))]

    [(list '#%testI-typed _ ...)
     ast]
    [(list '#%testE-typed _ ...)
     ast]
    
    [(list something ...)
     (map add-ids something)]
    
    [not-list ast]))

;; The actual type checker.

(define (full-type-checker ast same-type? same-type?no-convert
                           #:parent-field [parent-field #F] ;; wether or not you prioritize calling the field of the parent over that of the child
                           #:parent-method [parent-method #F]
                           #:array-subtyping [array-subtyping? #T]
                           #:show-type-func [show-type-func global-show-type]
                           )
  
  (set! ast (add-ids ast))

  (define-values (class/struct-ids instance-types level-env) (get-level-hashes ast (im-hash) (make-hash) built-in))
  
  (type-checker
   ast
   built-in
   class/struct-ids
   instance-types
   level-env
   (full-defined-h (hash->list built-in) (im-hash))
                
   (lambda (t1 t2) (same-type? t1 t2 instance-types))
   (lambda (t1 t2) (same-type?no-convert t1 t2 instance-types))
   
   (if parent-field hash-set-all-new hash-set-all)
   (if parent-method hash-set-all-new hash-set-all)
   
   array-subtyping? show-type-func
                ))

(define (get-level-hashes ast class/struct-ids instance-types env-hash)
  (define ids (get-class/struct-ids ast class/struct-ids))
  (make-full-env ast ids instance-types env-hash))

(define (get-class/struct-ids ast class/struct-ids)

  (match ast

    [(list (list '#%struct id name _ ...) rest ...)
     (get-class/struct-ids
      rest (hash-set class/struct-ids name (cons id 'struct)))]

    [(list (list '#%class id name _ ...) rest ...)
     (get-class/struct-ids
      rest (hash-set class/struct-ids name (cons id 'class)))]

    [(list _ rest ...)
     (get-class/struct-ids rest class/struct-ids)]

    [(list) class/struct-ids]))
  
(define (make-full-env ast class/struct-ids instance-types env-hash)

  (define make-type (default-make-type class/struct-ids instance-types))
  (define (recur-env rest new-env)
    (make-full-env rest class/struct-ids instance-types new-env))
  (define (recur-set-env rest name id type)
    (make-full-env rest class/struct-ids instance-types (hash-set env-hash name (cons id type))))
  
  (match ast
        
    [(list (list '#%typed-func id name (list (app make-type args) ...) (app make-type return-type) body ...) rest ...)
     (define type (func-type name (map arg-type args) return-type void))
     (recur-set-env rest name id type)]
        
    [(list (list '#%struct id name (list (app make-type args) ...)) rest ...)
     (hash-set! instance-types id (struct-type args))
     (define type (func-type name (map arg-type args) (nominal-struct-type name id) void))
     (recur-set-env rest name id type)]

    [(list (list '#%class id name parent-name (app make-type parent) fields/methods ...) rest ...)
     
     (define fields (filter is-field? fields/methods))
     (define methods (filter is-method? fields/methods))

     (define (special-make-type field/method)
       (match field/method
         [(list _ _id name type bind) (list name (make-type type))]))

     (define Fields:names+types (map special-make-type fields))
     (define Methods:names+types (map special-make-type methods))
     
     (hash-set! instance-types id (class-type Fields:names+types Methods:names+types parent id))
     (define type (func-type name '() (object-type name id) void))
     (recur-set-env rest name id type)]

    [(list (list (or '#%field '#%method) id name (app make-type type) bind) rest ...)
     (recur-set-env rest name id type)]
    
    [(list _ rest ...)
     (make-full-env rest class/struct-ids instance-types env-hash)]

    [(list) (values class/struct-ids instance-types env-hash)]
    ))

(define (full-defined-h level-list defined-hash)
  (match level-list
    [(list (cons key (cons id type)) rest ...)
     (full-defined-h rest (hash-set defined-hash (cons key id) #T))]
    [(list)
     defined-hash]))

(define (type-checker ast env-hash class/struct-ids instance-types full-level-env all-defined same-type? same-type?no-convert field-hash-func method-hash-func array-subtyping? show-type-func)
   
  ;; HELPER FUNCTIONS ;;
   
  (define (t-o val) (cdr (hash-ref env-hash val)))
  (define (is-defined? val) (hash-has-key? env-hash val))

  (define (all-change-recur new-ast new-env new-class/struct-ids new-instance-types new-full-level-env new-all-defined)
    (type-checker
     new-ast new-env new-class/struct-ids new-instance-types new-full-level-env new-all-defined ;; chaning values
     same-type? same-type?no-convert field-hash-func method-hash-func array-subtyping? show-type-func)) ;; constant values

  (define (ast-replace new-item)
    (set! ast (cons (car ast) (cons new-item (cdr ast)))))

  (define (recur-ast-single new-ast)
    (recur-ast (list new-ast)))
  (define (recur-ast new-ast)
    (recur-ast-env new-ast env-hash all-defined))
  (define (recur-ast-env new-ast new-env new-all-defined)
    (recur-ast-env-ids new-ast new-env class/struct-ids new-all-defined))
  (define (recur-ast-env-ids new-ast new-env new-class/struct-ids new-all-defined)
    (define-values (level-class/struct-ids level-instance-types level-env) (get-level-hashes new-ast new-class/struct-ids instance-types new-env))
    (all-change-recur new-ast new-env level-class/struct-ids level-instance-types level-env new-all-defined))
  
  (define (recur-cdr)
    (all-change-recur (cdr ast) env-hash class/struct-ids instance-types full-level-env all-defined))

  (define-syntax-rule (require-same-type t1 t2 error-message args ...)
    (require-same same-type? t1 t2 error-message args ...))
  (define-syntax-rule (require-same-type-no-convert t1 t2 error-message args ...)
    (require-same same-type?no-convert t1 t2 error-message args ...))
  
  (define-syntax-rule (require-same this.same-type? t1 t2 error-message args ...)
    (cond
      [(not (this.same-type? t1 t2)) (raise-error error-message args ...)]))

  (define (add-definition name id type)
    (set! env-hash (hash-set env-hash name (cons id type)))
    (set! all-defined (hash-set all-defined (cons name id) #T)))
  
  (define (full-defined)
    (full-defined-h (hash->list full-level-env) all-defined))
  
  (define (exists? type) (equal? type "NONE"))

  (define make-type (default-make-type class/struct-ids instance-types))

  (define (show-type type) (show-type-func instance-types type))

  (define (check-arg-type op position actual-type expected-type)
    (when (not (same-type? actual-type expected-type))
      (raise-error ERROR-STRING:wrong-argument-type
                   op (show-type expected-type) position (show-type actual-type))))

  (define (all-fields class)
    (class-all class field-hash-func 'fields instance-types))
  (define (all-methods class)
    (class-all class method-hash-func 'methods instance-types))

  (define (parent-ref name field/method)
    (define all-func (if (equal? 'method field/method) all-methods all-fields))
    (cond
      [(and
        (is-defined? "PARENT CLASS")
        (hash-has-key? (all-func (get-instance-type instance-types (t-o "PARENT CLASS"))) name))
       (hash-ref (all-func (get-instance-type instance-types (t-o "PARENT CLASS"))) name)]
      [else
       "NONE"]))

  ;; MAIN BODY ;;
  
  (define type
    (cond
      [(empty? ast) 'Void]
      [else
       (define item (car ast))
       (match item
 
         [(? string? item) 'String]
      
         [(? is-boolean? item) 'Boolean]
      
         [(list '#%number str) 'Number]
      
         ;; the tests type check the pieces of code they get individually,
         ;; so for the moment in the executable part of the test you can't reference anything outside of the test
         [(? is-testI? item)
          (check-arg-number 'testI item 2)
          
          (match item
            [(list _ (? string? expected) prog) 'Void]
            [else (check-arg-type 'testI 1 "regular executable" "string literal")])]
      
         [(? is-testE? item)
          (check-arg-number 'testE item 2)
          
          (match item
            [(list _ (? string? expected) prog) 'Void]
            [else (check-arg-type 'testE 1 "regular executable" "string literal")])]
      
         [(list '#%block body ...) (recur-ast body)]
      
         [(list '#%++ s1 s2)
          (check-arg-number '++ item 2)
          
          (define type1 (recur-ast-single s1))
          (define type2 (recur-ast-single s2))
          
          (check-arg-type "++" "1" type1 'String)
          (check-arg-type "++" "2" type2 'String)
          
          'String]

         [(list (? is-operator? op) v1 v2)
          (check-arg-number (subsymbol (car item) 2) item 2)
       
          (define operation (subsymbol op 2))
          (define type1 (recur-ast-single v1))
          (define type2 (recur-ast-single v2))

          (define boolean-operations (list '#%== '#%<= '#%>= '#%< '#%> '#%!=))
          (define return-type 
            (if (member? op boolean-operations) 'Boolean 'Number))

          (check-arg-type operation 1 type1 'Number)
          (check-arg-type operation 2 type2 'Number)

          return-type]
     
         [(list '#%if condition then else)
          (check-arg-number 'if item 3)
          
          (define condition-type (recur-ast-single condition))
          (define then-type (recur-ast-single then))
          (define else-type (recur-ast-single else))
          
          (require-same-type condition-type 'Boolean ERROR-STRING:non-boolean-condition (show-type condition-type))
          (require-same-type-no-convert then-type else-type ERROR-STRING:then-else-mismatch (show-type then-type) (show-type else-type))

          then-type]

         [(list '#%to-array (app make-type type) elements ...)

          (define index-on -1)
          (define this.same-type? (if array-subtyping? same-type? same-type?no-convert))

          (for-each
           (lambda (element-type)
             (increment index-on)
             (require-same this.same-type? element-type type
                    ERROR-STRING:wrong-array-element-type (show-type type) (show-type element-type) index-on))
           (map recur-ast-single elements))

          (array-type type)]

         [(list '#%array-access (app recur-ast-single array) index)
          
          (match array
            [(array-type type) type]
            [else (raise-error ERROR-STRING:not-an-array (show-type array))])]

         [(list '#%array-set (app recur-ast-single array) index (app recur-ast-single new-val))

          (define this.same-type? (if array-subtyping? same-type? same-type?no-convert))

          (cond
            [(not (array-type? array)) (raise-error ERROR-STRING:not-an-array-set (show-type array))]
            [(not (this.same-type? new-val (array-type-type array))) (raise-error ERROR-STRING:wrong-array-element-set-type (show-type (array-type-type array)) (show-type new-val))]
            [else (array-type-type array)])]
     
         [(list '#%struct id name (list (app make-type args) ...))
          
          (define arg-names (map arg-name args))
          (define arg-types (map arg-type args))
         
          (define type (func-type name arg-types (nominal-struct-type name id) void))

          (add-definition name id type)
          (add-definition (nominal-struct-type name id) id '_)
         
          'Void]

         [(list '#%method id name (app make-type type) (list '#%method-val args return-type body ...))

          (define parent-method-type (parent-ref name 'method))
          (define parent-field-type (parent-ref name 'field))
          
          (cond
            [(not (exists? parent-field-type))
             (raise-error ERROR-STRING:expected-field-given-method name)]
            [(nor (exists? parent-method-type) (same-type? type parent-method-type))
             (raise-error
              ERROR-STRING:parent-child-method-type-mismatch
              name (show-type type)
              name (show-type parent-method-type))
             ])

          (ast-replace (append (list '#%typed-func id name args return-type) body))
          
          'Void]
         
         [(list '#%field id name (app make-type type) bind)

          (define bind-type (recur-ast-single bind))
          (define parent-field-type (parent-ref name 'field))
          (define parent-method-type (parent-ref name 'method))

          (require-same-type bind-type type ERROR-STRING:wrong-decleration-type name (show-type type) (show-type bind-type))
          
          (cond
            [(not (exists? parent-method-type))
             (raise-error ERROR-STRING:expected-method-given-field name)]
            [(nor (exists? parent-field-type) (same-type? type parent-field-type))
             (raise-error
              ERROR-STRING:parent-child-field-type-mismatch
              name (show-type type)
              name (show-type parent-field-type))
             ])
          
          (add-definition name id type)
          
          'Void]

         [(list '#%class id name parent-name (app make-type parent) fields/methods ...)
          
          (cond
            [(not-parent? parent) (void)]
            [(not (is-defined? parent)) (raise-error ERROR-STRING:non-existant-parent (show-type name) (show-type parent))]
            [(not (object-type? parent)) (raise-error ERROR-STRING:non-class-parent (show-type name) (show-type parent))])
            

          (define fields (filter is-field? fields/methods))
          (define methods (filter is-method? fields/methods))

          (define parent-id (next-global-id))

          (define (make-pair field/method)
            (match field/method
              [(list field/method id name type bind) (list name (make-type type))]))
          
          (define Fields:names+types  (map make-pair fields))
          (define Methods:names+types (map make-pair methods))

          (define type (func-type name '() (object-type name id) void))
          (add-definition name id type)
          (add-definition (object-type name id) id '_)

          (define new-env-hash
            (if (not-parent? parent)
                env-hash
                (hash-set* env-hash
                           parent-name (cons parent-id parent)
                           "PARENT CLASS" (cons parent-id parent)
                           )))

          (define new-all-defined 
            (hash-set-if (not-parent? parent) all-defined (cons parent-name (next-global-id)) #T))
          
          (recur-ast-env fields/methods new-env-hash new-all-defined)
          
          ]
      
         [(list '#%typed-func id name (list (app make-type args) ...) (app make-type return-type) body ...)

          (define inner-type (cons id (func-type name (map arg-type args) return-type void)))

          (define ids (map (lambda (x) (next-global-id)) args))

          (define new-class/struct-ids
            (hash-set-all
             class/struct-ids
             (map
              (lambda (x) (match x [(list _id name (class-type fields methods parent id)) (cons name (cons id 'class))]))
              (filter
               (lambda (x) (match x [(list id name type) (class-type? type)]))
               (map cons ids args)))))
          
          (define (add-args env)
            (hash-set-all
             env
             (map (lambda (arg id) (cons (arg-name arg) (cons id (arg-type arg)))) args ids)))

          (define (add-defined-args defined-hash)
            (hash-set-all
             defined-hash
             (map (lambda (arg id) (cons (cons (arg-name arg) id) #T)) args ids)))
       
          (define new-env-hash
            (hash-set
             (add-args (combine-hash full-level-env env-hash))
             name inner-type))
          
          (define new-all-defined
            (hash-set (add-defined-args (full-defined)) (cons name id) #T))
          
          ;; This checks what happens when called from the end of it's ast level of the code 
          (define body-type
            (recur-ast-env-ids body new-env-hash new-class/struct-ids new-all-defined))

          (define (recall-function given-all-defined)
            
            (define full-env (add-args (only-defined new-env-hash given-all-defined)))
            (define new-all-defined (add-defined-args given-all-defined))
            
            (recur-ast-env-ids body full-env new-class/struct-ids new-all-defined))

          (define type (func-type name (map arg-type args) return-type recall-function))
          (add-definition name id type)
       
          (require-same-type body-type return-type
             ERROR-STRING:wrong-function-return-type name (show-type return-type) (show-type body-type))

          'Void]
      
         [(list '#%typed-let (list name (app make-type type) bind) body ...)
       
          (define bind-type (recur-ast-single bind))
          (define id (next-global-id))
          
          (require-same-type bind-type type ERROR-STRING:wrong-decleration-type name (show-type type) (show-type bind-type))

          (define new-env-hash (hash-set env-hash name (cons id type)))
          (define new-class/struct-ids (hash-set-if (class-type? type) class/struct-ids name (cons (class-type-id type) 'class)))
          (define new-all-defined (hash-set all-defined (cons name id) #T))

          (define body-type (recur-ast-env-ids body new-env-hash new-class/struct-ids new-all-defined))
          body-type]

         [(list '#%reassign variable new-bind)

          (define variable-type (t-o variable))
          (define bind-type (recur-ast-single new-bind))

          (require-same-type bind-type variable-type ERROR-STRING:wrong-reassignment-type variable (show-type bind-type) (show-type variable-type))

          variable-type]

         [(list '#%reassign-field str/obj field new-bind)

          (define field-type (recur-ast-single (list '#%field-id str/obj field)))
          (define bind-type (recur-ast-single new-bind))

          (require-same-type bind-type field-type ERROR-STRING:wrong-reassignment-type field (show-type bind-type) (show-type field-type))

          field-type]
         
         [(list '#%field/method-id instance e field/method)

          (define instance-type (recur-ast-single instance))

          (cond
            [(nor (nominal-struct-type? instance-type) (object-type? instance-type))
             (raise-error ERROR-STRING:not-a-struct-or-object e (show-type instance-type))])

          (define struct/class-type (get-instance-type instance-types instance-type))

          (define method? (equal? field/method 'method))
          
          (define ERROR-STRING:unbound-error (if method? ERROR-STRING:unbound-method ERROR-STRING:unbound-field))
          (define class-all (if method? all-methods all-fields))
          (define hash-set-func (if method? method-hash-func field-hash-func))
          
          (define field/method-hash
            (if (struct-type? struct/class-type)
                (struct-all-fields struct/class-type)
                (class-all struct/class-type) ;; sometimes it should be hash-set-all-new
                ))
          
          (cond
            [(not (hash-has-key? field/method-hash e))
             (raise-error ERROR-STRING:unbound-error (show-type instance-type) e)])
          
          (hash-ref field/method-hash e)]

         [(list '#%field-id instance e)
          (ast-replace (list '#%field/method-id instance e 'field))
          'Void]

         [(list '#%method-call instance e args ...)
          (ast-replace (cons (list '#%field/method-id instance e 'method) args))
          'Void]
      
         [(list '#%id v)
          (if (is-defined? v)
              (t-o v) (raise-error ERROR-STRING:unbound-id v))]
      
         [(list func args ...)
          
          (define expected-type (recur-ast-single func))

          (cond
            [(nor (func-type? expected-type) (class-type? expected-type))
             (raise-error ERROR-STRING:not-a-function (show-type expected-type))])
          
          (define-values (func-name arg-types return-type)
            (match expected-type
              [(func-type name args return _checker) (values name args return)]
              [(class-type fields methods parent id) (values '<procedure> (list) (object-type '_ id))]
              ))

          (define real-types (map recur-ast-single args))

          (check-arg-number func-name item (length arg-types))

          (define arg# 0)
          
          (for-each
           (lambda (real-type arg-type)
             (increment arg#)
             (check-arg-type func-name arg# real-type arg-type))
           real-types arg-types)
                  
          (cond
            [(func-type? expected-type) ((func-type-checker expected-type) all-defined)])
          return-type]
         [else
          (raise-error "(internal error. please report this message to the brown TAs. Debugging info: ~s" ast)]
         )
       ]))

  ;; Just iterating to the next item in the ast
  (if (or (empty? ast) (empty? (cdr ast)))
      type
      (recur-cdr)))

(define-syntax-rule (#%typed-let (name type bind) body ...)
  (let ([name bind]) body ...))

(define-syntax-rule (#%typed-func name ((arg type) ...) return-type body ... lastBody)
  (define (name arg ...)
    (check-decrement-fuel) body ... lastBody))

(define-syntax-rule (#%struct struct-name ((e-name _type) ...))
  (begin
    (define symbols (list (quote e-name) ...))
    (define id (next-global-id))
    (define name (quote struct-name))
    
    (define (struct-name e-name ...) ;; the constructor
      (define vals (list e-name ...)) ;; the values given to the 'struct-name' constructor
      (define h (make-hash (map cons symbols vals)))
      (define len (hash-count h))

      (define get-func (lambda (e) (hash-ref h e)))
      (define set-func (lambda (e val) (hash-set! h e val)))
      
      (ml-struct name get-func set-func vals id))))

(define ERROR-STRING:expected-struct/object "Field/method reference expects a struct or object to be referenced from but given ~a.")

(define-syntax-rule (#%field-id instance e)
  (let ([instance-value instance])
    (cond
      [(ml-struct? instance-value)
       ((ml-struct-get-func instance-value) (quote e))]
      [(ml-object? instance-value)
       ((ml-object-get-func instance-value) (quote e) '#%field)]
      [else
       (raise-error ERROR-STRING:expected-struct/object instance-value)]
      )))
(define-syntax-rule (#%method-call instance e args ...)
  (let ([instance-value instance])
    (cond
      [(ml-struct? instance-value)
       ((#%field-id instance-value e) args ...)]
      [(ml-object? instance-value)
       (((ml-object-get-func instance-value) (quote e) '#%method) args ...)]
      [else
       (raise-error ERROR-STRING:expected-struct/object instance)]
      )))
(define-syntax-rule (#%reassign value new-bind)
  (begin
    (set! value new-bind)
    value))
(define-syntax-rule (#%reassign-field instance field new-bind)
  (let ([instance-value instance])
    (cond
      [(ml-struct? instance-value)
       ((ml-struct-set-func instance-value) (quote field) new-bind)]
      [(ml-object? instance-value)
       ((ml-object-set-func instance-value) (quote field) new-bind)]
      [else
       (raise-error ERROR-STRING:expected-struct/object instance-value)]
      )
    new-bind))
  



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utilities

(define-syntax-rule (raise-error string args ...)
  (raise-user-error (~a (format string args ...))))

(define-syntax-rule (subsymbol symbol args ...)
  (string->symbol (substring (symbol->string symbol) args ...)))

(define (hash-set-all hash pairs)
  (if (empty? pairs)
      hash
      (hash-set (hash-set-all hash (cdr pairs))
                (caar pairs) (cdar pairs))))
(define (hash-set-all-new hash pairs)
  (cond
    [(empty? pairs) hash]
    [(hash-has-key? hash (caar pairs)) (hash-set-all hash (cdr pairs))]
    [else (hash-set (hash-set-all hash (cdr pairs))
                    (caar pairs) (cdar pairs))]))

(define (first-false-index fun? l1 l2)
  (cond
    [(or (empty? l1) (empty? l2)) -1]
    [(not (fun? (first l1) (first l2))) 0]
    [else (+ 1 (first-false-index fun? (cdr l1) (cdr l2)))]))

(define (first-true fun? list)
  (cond
    [(empty? list) #f]
    [(fun? (first list)) (first list)]
    [else (first-true fun? (rest list))]))

(define-syntax-rule (hash-set-if condition hash key val)
  (cond
    [condition (hash-set hash key val)]
    [else hash]))

(define-syntax-rule (set!+ variable amount)
  (set! variable (+ variable amount)))
(define-syntax-rule (increment variable)
  (set!+ variable 1))