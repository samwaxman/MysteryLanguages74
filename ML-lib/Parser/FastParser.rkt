#lang racket
(require megaparsack megaparsack/text)
(require data/monad)
(require data/applicative)
(require (for-syntax syntax/parse))
(require syntax/strip-context)
(provide ML-parse)

(define (error-whitespace-insensitive or)
  (or/p
   or
   (do
       (hidden/p space/p)
     (error-whitespace-insensitive or))))
(define (e-nl/p or)
  (or/p
   (do
       (label/p "newline" (char/p #\newline))
     (ew-i/p
      or))
   (do
       space/p
     (e-nl/p or))))
(define (ew-i/p or)
  (error-whitespace-insensitive or))
(define-syntax-rule (if-def-parse syn parser)
  (do (if syn parser (fail/p (message (srcloc #f #f #f #f #f) #f '())))))

(define acceptable-identifier-chars
  (list->string
   (list #\_ #\- #\+ #\\ #\*)))

(define-syntax-rule (? rule)
  (many/p rule #:min 0 #:max 1))
(define-syntax-rule (.... rule)
  (many/p rule #:min 0 #:max +inf.0))
(define-syntax-rule (..1 rule)
  (many/p rule #:min 1 #:max +inf.0))
(define (string-in/p list)
  (apply or/p (map (lambda (x) (try/p (string/p x))) list)))



(define (default-stringify list)
  (foldl (lambda (x a) (~a a x)) "" list))
(define d default-stringify)


(define NaN/p
  (do
      (string/p "NaN")
    (pure (list #'#%number "NaN"))))
(define comment/p
  (do
      (or/p (char/p #\;) (char/p #\#))
    (many/p (satisfy/p (lambda (x) (not (eq? x #\newline)))))))
(define number/p
  (or/p
   NaN/p
   (do
       [sign <- (? (char/p #\-))]
     (or/p
      (do
          (string/p "Inf")
        (if (null? sign)
            (pure (list #'#%number "Inf"))
            (pure (list #'#%number "-Inf"))))
      (do
          [int <- integer/p]
        [decimal <- (? (do
                           (char/p #\.)
                         [deci <- (many/p digit/p #:min 1)]
                         (pure (~a "." (d deci)))))]
        (pure (list #'#%number (d (list (d sign) int (d decimal))))))))))



(define whitespace-insensitive/p
  (hidden/p (many/p space/p #:min 0 #:max +inf.0)))
(define w-i/p
  whitespace-insensitive/p)
(define at-least-one-whitespace/p
  (hidden/p (many/p space/p #:min 1 #:max +inf.0)))
(define w/p
  at-least-one-whitespace/p)
(define at-least-one-newline/p
  (label/p "newline" (or/p
                      (char/p #\newline)
                      (do
                          space/p
                        at-least-one-newline/p))))
(define n/p
  at-least-one-newline/p)

(define special-chars/p
  (do
      (string/p "\\\"")
    (pure #\")))
;;STRING PARSING
(define regular-chars/p
  (or/p
   (char-between/p #\space #\!)
   (char-between/p #\# #\[)
   (char-between/p #\] #\~)))
(define string-inside/p
  (label/p "a text char or a finishing quote"
           (or/p
            (do
                (string/p "\"")
              (pure (list)))
            (do
                [chars <- (or/p regular-chars/p special-chars/p)]
              [rest <- string-inside/p]
              (pure (cons chars rest))))))
;;Need to transform this so that when a special char is read
;We take off a \
(define user-string/p
  (do
      (string/p "\"")
    [text <- string-inside/p]
    (pure (list->string text))))



(define (ML-parse src in language [module-name 'mod-name]
                  #:let [ML-let #f]
                  #:if [ML-if #f]
                  #:func [ML-func #f]
                  #:reassign [ML-reassign #f]
                  #:lambda [ML-lambda #f]
                  #:record [ML-record #f]
                  #:+ [ML+ #f]
                  #:- [ML- #f]
                  #:/ [ML/ #f]
                  #:* [ML* #f]
                  #:++ [ML++ #f]
                  #:== [ML== #f]
                  #:!= [ML!= #f]
                  #:> [ML> #f]
                  #:< [ML< #f]
                  #:<= [ML<= #f]
                  #:>= [ML>= #f]
                  #:for [ML-for #f]
                  #:list [ML-list #f]
                  #:id [ML-id #f]
                  #:runAll [runAll #f]
                  #:arrays [ML-arrays #f]
                  #:typed-let [ML-typed-let #f]
                  #:typed-func [ML-typed-func #f]
                  #:struct [ML-struct #f]
                  #:object [ML-object #f]
                  #:first-class-classes [ML-higher-order-types #f]
                  #:typed-arrays [ML-typed-arrays #f]
                  )
  
  
  ;(printf "~nsrc: ~s~nin: ~s~nlanguage: ~s~n~n" src in language)
  ;(set! ML-func #f)

  
  (define (unacceptable-ids)
    (foldl (lambda (x acc) (if (car x) (cons (cdr x) acc) acc))
           '()
           (list (cons ML-lambda 'lam) (cons ML-func 'fun)
                 (cons ML-if 'if) (cons ML-if 'true)
                 (cons ML-if 'false) (cons ML-for 'for)
                 (cons true 'end) (cons ML-typed-func 'fun)
                 (cons true 'block) (cons ML-struct 'struct)
                 (cons ML-object 'class) (cons ML-object 'var))))

  (define primative-identifier-types
    (list 'Number 'String 'Boolean))
  
  ;;Wrap this in another parser that disallows certain identifiers
  ;;like our binaries, true and false, =, etc.
  
  
  
  (define raw-identifier/p
    (label/p "identifier" (try/p (guard/p (or/p
                                           (do
                                               [start <- letter/p]
                                             [continue <- (label/p "identifier char" (many/p (or/p (char-in/p acceptable-identifier-chars)
                                                                                                   letter/p
                                                                                                   digit/p) #:min 0 #:max +inf.0))]
                                             (guard/p (pure (string->symbol (d (list start (d continue)))))
                                                      (lambda (x) (foldl (lambda (y acc) (and acc (not (eq? y x)))) #t
                                                                         (unacceptable-ids)))))
                                           (do
                                               [start <- (char-in/p acceptable-identifier-chars)]
                                             [continue <- (label/p "identifier char" (many/p (or/p (char-in/p acceptable-identifier-chars)
                                                                                                   letter/p
                                                                                                   digit/p) #:min 0 #:max +inf.0))]
                                             (guard/p (pure (string->symbol (d (list start (d continue)))))
                                                      (lambda (x) (and
                                                                   (foldl (lambda (x acc) (or acc (char-alphabetic? x))) #f (string->list (symbol->string x)))
                                                                   (not (member (symbol->string x) (dict-keys binary-op-builtins))))))))
                                          (lambda (x) (not (member x (list 'Inf '-Inf 'NaN))))))))

  (define identifier/p
     (do
         [id <- raw-identifier/p]
       (pure (list #'#%id id))))
  
  (define (strip-id id)
    (cadr (syntax->list id)))

  
  (define (function/p)
    (do
        (string/p "fun")
      w/p
      [fun-name <- (syntax/p raw-identifier/p)]
      [fun-arguments <- (syntax/p (error-sequence/p raw-identifier/p))]
      w-i/p
      (char/p #\:)
      w-i/p
      [block <- (syntax/p (block/p))]
      ;NOT DOING THIS GETS YOU INTO BIG TROUBLE:
      ;1: HIGHLIGHTING WILL BE OFF
      ;2: SYNTAX PARAMETERS will take the WRONG VALUE
      
      ;This says, keep your regular context, but know that your location
      ;is the location of the fun string
      (pure (append (list #'#%func fun-name fun-arguments) block))))


  (define (class-sequence-internal/p single)
    (do
        [arg <- (syntax/p single)]
      (ew-i/p
       (or/p
        (do
            (char/p #\,)
          (ew-i/p
           (do
               [rest <- (syntax/p (class-sequence-internal/p single))]
             (pure (cons arg rest)))))
        (do
            (char/p #\])
          (pure (list arg)))))))

  (define (class-body-type/p)
    (or/p
     (do
         (char/p #\])
       (pure (list)))
        (class-specs/p)))

  (define (class-specs/p)
    (do
    (char/p #\[)
    w-i/p
      (or/p
       (do
           [vars <- (syntax/p (class-vars/p))]
         w-i/p
         (or/p
          (do
              (char/p #\])
          (pure (list vars)))
          (do
              (char/p #\[)
              [funs <- (syntax/p (class-funs/p))]
            w-i/p
             (char/p #\])
            (pure (list vars funs)))))
       (do
           [funs <- (syntax/p (class-funs/p))]
         w-i/p
         (or/p
          (do
              (char/p #\])
            (pure (list funs)))
          (do
              (char/p #\[)
              [vars <- (syntax/p (class-vars/p))]
            w-i/p
             (char/p #\])
            (pure (list vars funs))))))))

  (define (class-vars/p)
    (do
    (string/p "Vars")
      w-i/p
      (char/p #\:)
      w-i/p
      (or/p
       (do
           (char/p #\])
         (pure (list  #'#%field-types)))
       (do
     [vars <- (syntax/p (class-sequence-internal/p (typed-argument/p)))]
         (pure (cons #'#%field-types vars))))))

  (define (fun-type/p)
    (do
     [fun-name <- (syntax/p raw-identifier/p)] ;; Gets fun-name with same syntax as regular fun
      [fun-arguments <- (syntax/p (error-sequence/p (type/p)))] ;; Finds args in same way as regular fun except uses typed-argumment instead to deal with typed args
      w-i/p
      (string/p "->")
      w-i/p
      [return-type <- (syntax/p (type/p))] ;; Gets the type after the arrow
      (pure (list fun-name fun-arguments return-type))))
    
     

  (define (class-funs/p)
    (do
    (string/p "Funs")
      w-i/p
      (char/p #\:)
      w-i/p
      (or/p
       (do
           (char/p #\])
         (pure (list #'method-types)))
       (do
           [funs <- (class-sequence-internal/p (fun-type/p))]
         (pure (cons #'#%method-types funs))))))
     
            
          
        
        

  (define (class-type/p)
    (do
        (char/p #\[)
      w-i/p
        (string/p "Class")
      w-i/p
      (char/p #\:)
      w-i/p
      [fields+methods <- (syntax/p (class-body-type/p))]
      (pure (cons #'#%class-type fields+methods))))

  (define (array-type/p)
    (do
        (try/p (string/p "Array<"))
      [type <- (syntax/p (type/p))]
      (char/p #\>)

      (pure (list #'#%array-type type))))

  (define (type/p)
    (or/p
     (if-def-parse ML-higher-order-types (class-type/p))
     (if-def-parse ML-typed-arrays (array-type/p))
     (do
         [type <- (syntax/p raw-identifier/p)]
       (pure type))))
   
  (define (typed-argument/p)
     ;; Changes a typed argument of form "arg :: Type" to a list(arg Type) (Q: is it a prob that this is a list? should it be syntax like #'(arg type)?)
    (do
        [argument-name <- (syntax/p raw-identifier/p)] ;; Should find the arg
      w-i/p
      (string/p "::") ;; Seperates arg and Type. We can throw this away
      w-i/p
      [type <- (syntax/p (type/p))] ;; Should get the type
      
      (pure (list argument-name type)))) ;; Puts it into easier form to deal with. (Q: put #'typed-arg. Prob not b/c used by another /p)
  
  (define (argument-type/p)
     ;; Changes a typed argument of form "arg :: Type" to a list(arg Type) (Q: is it a prob that this is a list? should it be syntax like #'(arg type)?)
    (do
        [argument-name <- (syntax/p raw-identifier/p)] ;; Should find the arg
      w-i/p
      (string/p "::") ;; Seperates arg and Type. We can throw this away
      w-i/p
      [type <- (syntax/p (type/p))] ;; Should get the type
      
      (pure type))) ;; Puts it into easier form to deal with. (Q: put #'typed-arg. Prob not b/c used by another /p)
  
  (define (typed-function/p)
    ;; Converts a typed function into the readable type
    (do
        (string/p "fun") ;; Looks for fun to start function
      w/p
      [fun-name <- (syntax/p raw-identifier/p)] ;; Gets fun-name with same syntax as regular fun
      [fun-arguments <- (syntax/p (error-sequence/p (typed-argument/p)))] ;; Finds args in same way as regular fun except uses typed-argumment instead to deal with typed args
      w-i/p
      (string/p "->")
      w-i/p
      [return-type <- (syntax/p (type/p))] ;; Gets the type after the arrow
      w-i/p
      (char/p #\:)
      w-i/p
      [block <- (syntax/p (block/p))] ;; body of fun same as regular func

      (pure (append (list #'#%typed-func fun-name fun-arguments return-type) block))))

  (define (field/p)
    (do
        (string/p "var")
      w-i/p
      [field-name <- (syntax/p field/method-name/p)]
      w-i/p
      (string/p "::")
      w-i/p
      [type <- (syntax/p (type/p))]
      w-i/p
      (char/p #\=)
      w-i/p
      [bind <- (syntax/p argument-position/p)]
      w-i/p
      (string/p "end")

      (pure (list #'#%field field-name type bind))))

  (define (method/p)
    (do
        (string/p "fun") ;; Looks for fun to start function
      w/p
      [fun-name <- (syntax/p field/method-name/p)] ;; Gets fun-name with same syntax as regular fun
      [fun-arguments <- (syntax/p (error-sequence/p (typed-argument/p)))] ;; Finds args in same way as regular fun except uses typed-argumment instead to deal with typed args
      w-i/p
      (string/p "->")
      w-i/p
      [return-type <- (syntax/p (type/p))] ;; Gets the type after the arrow
      w-i/p
      (char/p #\:)
      w-i/p
      [block <- (syntax/p (block/p))] ;; body of fun same as regular func
      
      (pure (list #'#%method fun-name
                  (list #'#%method-type fun-name fun-arguments return-type)
                  (append (list #'#%method-val fun-arguments return-type) block)
                  ))))

  (define (class-no-inheritence/p)
    (do
        (char/p #\:)

      (pure (list void))))

  (define (class-yes-inheritence/p)
    (do
        (string/p "extends")
      w-i/p
      [parent-class-name <- (syntax/p raw-identifier/p)]
      w-i/p
      (char/p #\:)

      (pure (list parent-class-name))))

  (define class/struct-name/p  (guard/p raw-identifier/p (λ (x) (not (ormap (lambda (type) (equal? x type)) primative-identifier-types)))
                           "Cannot be a primatvie type"))
  (define field/method-name/p  (guard/p raw-identifier/p (λ (x) (not (equal? x 'super)))
                           "Fields/Methods can't be named super"))
  
  (define (class/p)
    (do
        (string/p "class")
      w-i/p
      [class-name <- (syntax/p class/struct-name/p)]
      w-i/p
      ;[parent-name <- (syntax/p (or/p (try/p (class-no-inheritence/p)) (class-yes-inheritence/p)))]
      [parent-name <- (or/p (syntax/p (class-no-inheritence/p)) (syntax/p (class-yes-inheritence/p)))]
      w-i/p
      [body <- (syntax/p (class-block/p))]

      (pure (append (list #'#%class class-name #'super parent-name) body))))
  
  (define (struct/p)
    (do
        (string/p "struct")
      w/p
      [struct-name <- (syntax/p class/struct-name/p)]
      w-i/p
      [struct-arguments <- (syntax/p (error-sequence/p (typed-argument/p)))]
      w-i/p
      (string/p "end")
      (pure (list #'#%struct struct-name struct-arguments))))

      
  
  
  ;;;;INFIX NOTATION PARSING ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;precednce levels (does not support right assosciative)
  (define (precedence-level op)
    (or ;this is probs how cases works, but why on earth did you do this?
     (and (eq? op "(") -inf.0)
     (and (member op high-precedence-ops) 2)
     (and (member op middle-precedence-ops) 1)
     (and (member op (dict-keys binary-op-builtins)) 0)))
  (define (op->syntax str-op)
    (datum->syntax #'str-op (string->symbol (string-append "#%" str-op))))
  
  ;now that the values ar ejust true and false, later you can just make this the dict keys instead of the whole thing
  (define binary-op-builtins
    (foldr (lambda (x acc) (if (cdr x) (cons x acc) acc)) '() (list (cons "++" ML++) (cons "+" ML+) (cons "-" ML-) (cons "/" ML/) (cons "*" ML*)
                                                                    (cons "!=" ML!=) (cons "<=" ML<=) (cons ">=" ML>=) (cons ">" ML>) (cons "<" ML<)
                                                                    (cons "==" ML==))))
  (define high-precedence-ops
    (list "*" "/"))

  (define middle-precedence-ops
    (list "+" "-"))
  
  (define operator/p
    (label/p "operator" (do
                            w/p
                          [op <- (string-in/p (dict-keys binary-op-builtins))]
                          w/p
                          (pure op))))
  (define whitespace-prefixed-op/p
    (label/p "operator" (do
                            [op <- (string-in/p (dict-keys binary-op-builtins))]
                          [spaces <- w/p]
                          (if-def-parse (cons? spaces) (pure op)))))
  
  
  (define (to-prefix-notation postfix-list)
    (define (helper h-list)
      (if (member (car h-list) (dict-keys binary-op-builtins))
          (let-values ([(first f-processed) (helper (cdr h-list))])
            (let-values ([(second s-processed) (helper (drop h-list (+ f-processed 1)))])
              (values (reverse (list first second (op->syntax (car h-list)))) (+ 1 f-processed s-processed))))
          (values (car h-list) 1)))
    ;it's never null but this is needed so that the monads don't go crazy on ya
    (if (null? postfix-list) ""
        (let-values ([(prefix _) (helper postfix-list)])
          prefix)))
  (define (move-operations op op-list output)
    (if (or (null? op-list)
            (> (precedence-level op)
               (precedence-level (car op-list))))
        (values (cons op op-list) output)
        (move-operations op (cdr op-list) (cons (car op-list) output))))
  (define (right-paren ops out)
    (cond
      [(null? ops) (values #f #f)]
      [(eq? (car ops) "(") (values ops out)]
      [else (right-paren (cdr ops) (cons (car ops) out))]))
  (define (binary-op/p single [start-expr (list)] [start-op (list)])
    (do
        [binary <- (guard/p (postfix-list/p single start-expr start-op)
                            (lambda (x) (and
                                         (car x)
                                         (cons? (cdr x))
                                         (not (member "(" (cdr x)))
                                         (or
                                          (member (car (cdr x)) (dict-keys binary-op-builtins))
                                          (= (length (cdr x)) 1)))))]
      (let ([final (to-prefix-notation (cdr binary))])
        (pure final))))
  (define (postfix-list/p single-parse [start-expr (list)] [start-op (list)])
    (define (helper output operations last-expr?)
      (or/p
       ;if you encounter right paren, let the helper take care of it and continue
       (try/p (if-def-parse last-expr? (do
                                 w-i/p
                               (char/p #\))
                               (let-values ([(op-list out) (right-paren operations output)])
                                 (if (and op-list out)
                                     (or/p
                                      ;this might mess up error reporting for this one case
                                      (try/p (do
                                                 [op <- (label/p "operator" operator/p)]
                                               (let-values ([(op-list* out*) (move-operations op (cdr op-list) out)])
                                                 (try/p (helper out* op-list* #f)))))
                                      (helper out (cdr op-list) #t))
                                     (fail/p (message (srcloc #f #f #f #f #f) ")" '())))))))
       ;Otherwise, parse an expr
       (do
           [expr1 <- (syntax/p single-parse)]
         (or/p
          ;see if there's another op after it.
          ;if so, move-operations and continue
          (try/p (do
                     [opr1 <- (label/p "operator" operator/p)]
                   (let-values ([(op-list out) (move-operations opr1 operations (cons expr1 output))])
                     (helper out op-list #f))))
          ;otherwise, there might be a right-paren so check that with the helper
          (helper (cons expr1 output) operations #t)))
       ;if no expressions were found, see if there's a left-paren
       (if-def-parse (not last-expr?) (try/p (do
                  w-i/p
                (char/p #\()
                (helper output (cons "(" operations) #f))))
       ;if not we're done.
       (pure (cons last-expr? (append (reverse operations) output)))))
    (helper start-expr start-op #f))
  
  (define lambda/p
    (do
        (string/p "lam")
      w-i/p
      (char/p #\()
      [args <- (syntax/p (many/p (syntax/p raw-identifier/p) #:sep (do w-i/p (char/p #\,) w-i/p)))]
      w-i/p
      (char/p #\))
      w-i/p
      (char/p #\:)
      w-i/p
      [body <- (syntax/p (block/p))]
      (pure (append (list #'#%lambda args) body))))
  
  (define record/p
    (do
        (char/p #\{)
      w-i/p
      [entries <- (many/p (do
                              [field <- (syntax/p argument-position/p)]
                            w-i/p
                            (char/p #\:)
                            w-i/p
                            [binding <- (syntax/p argument-position/p)]
                            w-i/p
                            (pure (list field binding)))
                          #:sep (do w-i/p (char/p #\,) w-i/p))]
      (char/p #\})
      (pure (list #'#%record entries))))
  
  
  (define user-block/p
    (do
        [syn-block <- (syntax/p (string/p "block"))]
      w-i/p
      (char/p #\:)
      w-i/p
      [block <- (block/p)]
      (let ([decorated-let (datum->syntax #'let
                                          'let
                                          syn-block)])            
        (pure (cons #'#%block block)))))
  
  (define (if/p)
    (do
        (string/p "if")
      w-i/p
      [condition <- (syntax/p argument-position/p)]
      w-i/p
      (char/p #\:)
      w-i/p
      [then <- (syntax/p argument-position/p)]
      w-i/p
      (string/p "else")
      w-i/p
      (char/p #\:)
      w-i/p
      [else <- (syntax/p argument-position/p)]
      w-i/p
      (string/p "end")
      (pure (list #'#%if condition then else))))
  
  
  
  (define (top-level-block/p)
    (ew-i/p
     (or/p
      (do
          comment/p
        (top-level-block/p))
      (do
          (label/p "end of input" eof/p)
        (pure (list)))
      (block-position/p (label/p "end of input"  eof/p)))))
  (define (block/p)
    (block-position/p (try/p (string/p "end"))))
  (define (class-block/p)
    (class-block-position/p (try/p (string/p "end"))))
  
  (define (list-internal/p)
    (do
        [arg <- (syntax/p argument-position/p)]
      (ew-i/p
       (or/p
        (do
            (char/p #\])
          (pure (list arg)))
        (do
            (char/p #\,)
          (ew-i/p
           (do
               [rest <- (syntax/p (list-internal/p))]
             (pure (cons arg rest)))))))))
  (define (list/p)
    (do
        (char/p #\[)
      [syn-list <- (syntax/p (string/p "list:"))]
      (ew-i/p
       (or/p
        (do
            (char/p #\])
          (pure (list #'#%list)))
        (do
            [internal <- (syntax/p (list-internal/p))]
          (pure (cons #'#%list internal)))))))

  (define (array/p)
    (do
        (char/p #\[)
      [syn-list <- (syntax/p (string/p "array:"))]
      (ew-i/p
       (or/p
        (do
            (char/p #\])
          (pure (list #'#%to-array)))
        (do
            [internal <- (syntax/p (list-internal/p))]
          (pure (cons #'#%to-array internal)))))))

  (define (typed-array/p)
    (do
        (char/p #\[)
      (syntax/p (string/p "array"))
      w-i/p
      (string/p "::")
      w-i/p
      [type <- (syntax/p (type/p))]
      w-i/p
      (char/p #\:)
      (ew-i/p
       (or/p
        (do
            (char/p #\])
          (pure (list #'#%to-array type)))
        (do
            [internal <- (syntax/p (list-internal/p))]
          (pure (cons #'#%to-array (cons type internal))))))))
  
  (define for/p
    (do
        (try/p (string/p "for"))
      (try/p space/p)
      (ew-i/p
       (do
           [id <- (syntax/p raw-identifier/p)]
         space/p
         (ew-i/p
          (do
              (string/p "in")
            (ew-i/p
             (do
                 [in-list <- (syntax/p argument-position/p)]
               (ew-i/p
                (do
                    (char/p #\:)
                  (ew-i/p
                   (do
                       [block <- (syntax/p (block-position/p (try/p (string/p "end"))))]
                     (pure (append (list #'#%for id in-list) block))))))))))))))

   (define (reassign-id/p id)
    (do
        [binding <- (syntax/p argument-position/p)]
      (pure (list #'#%reassign id binding))))

  (define get-reassign-id
    (do
        [id <- (syntax/p raw-identifier/p)]
      w-i/p
      (string/p ":=")
      (pure id)))

  (define reassign/p
    (do
        [id <- (try/p (syntax/p get-reassign-id))]
      w-i/p
      (reassign-id/p id)))
  
  ;not really prims I haven't changed the name yet.
  ;More like expressions, but without function apps.
  (define primitive/p
    (or/p
     (if-def-parse ML-reassign reassign/p)
     (if-def-parse ML-arrays (array/p))
     (if-def-parse ML-typed-arrays (typed-array/p))
     (if-def-parse ML-list (list/p))
     (if-def-parse ML-for for/p)
     (label/p "string" user-string/p)
     (try/p (label/p "identifier" identifier/p))
     (try/p (label/p "number" number/p))
     user-block/p
     (if-def-parse ML-lambda lambda/p)
     (if-def-parse ML-record record/p)
     (if-def-parse ML-if (if/p))
     (try/p (if-def-parse ML-if (or/p (do
                                          (string/p "true")
                                        (pure 'true))
                                      (do
                                          (string/p "false")
                                        (pure 'false)))))))
  
  
  
  (define (error-sequence-internal/p single)
    (do
        [arg <- (syntax/p single)]
      (ew-i/p
       (or/p
        (do
            (char/p #\,)
          (ew-i/p
           (do
               [rest <- (syntax/p (error-sequence-internal/p single))]
             (pure (cons arg rest)))))
        (do
            (char/p #\))
          (pure (list arg)))))))
  (define (error-sequence/p single)
    (do
        (char/p #\()
      (ew-i/p
       (or/p
        (do
            (char/p #\))
          (pure (list)))
        (error-sequence-internal/p single)))))
  
  (define (postfix/p arg)
    (or/p
     (if-def-parse
      ML-record
      (do
         (char/p #\[)
       w-i/p
       [accessor <- (syntax/p argument-position/p)]
       w-i/p
       (char/p #\])
       (pure (cons #t (list #'#%access arg accessor)))))
     (if-def-parse
      (or ML-struct ML-object)
      (try/p
       (do
           (char/p #\.)
         [el <- raw-identifier/p]
         [arguments <- (syntax/p (error-sequence/p argument-position/p))]
         (pure (cons #t (append (list #'#%method-call arg el) arguments))))))
     (if-def-parse
      (or ML-struct ML-object)
      (do
         (char/p #\.)
        [el <- raw-identifier/p]
       (pure (cons #t (list #'#%field-id arg el)))))
     (do
         [application <- (syntax/p (error-application-args/p))]
       (pure (cons #f (foldl (lambda (x acc) (cons acc x)) arg (syntax->list application)))))
     (if-def-parse
      ML-typed-arrays
      (try/p
       (do
           (char/p #\[)
         w-i/p
         [index <- (syntax/p argument-position/p)]
         w-i/p
         (char/p #\])
         w-i/p
         (string/p ":=")
         w-i/p
         [value <- (syntax/p argument-position/p)]
         (pure (cons #f (list #'#%array-set arg index value))))))    
     (if-def-parse
      (or ML-arrays ML-typed-arrays)
      (do
          (char/p #\[)
        w-i/p
        [index <- (syntax/p argument-position/p)]
        w-i/p
        (char/p #\])
        (pure (cons #f (list #'#%array-access arg index)))))))
  
  (define (error-application-args/p)
    (do
        [args <- (syntax/p (error-sequence/p argument-position/p))]
      (or/p
       (do
           [rest <- (syntax/p (error-application-args/p))]
         (pure (cons args rest)))
       (pure (list args)))))

  (define (collect-postfixes/p arg record-a?)
    (or/p
     (do
         [postfixed <- (syntax/p (postfix/p arg))]
       (collect-postfixes/p (cdr (syntax->list postfixed)) (car (syntax->list postfixed))))
     (pure (cons record-a? arg))))
  
  (define rec-reassign/p
    (do
    (try/p
     (do w-i/p (string/p ":=")))
      w-i/p
     argument-position/p))
  (define single-expression/p
    (do
        [prim <- (syntax/p primitive/p)]
      [postfixed <- (collect-postfixes/p prim #'#f)]
      (if (syntax->datum (car postfixed))
          (or/p
           (do
               [new-val <- (syntax/p rec-reassign/p)]
             (pure (list #'#%reassign-field (second (cdr postfixed)) (third (cdr postfixed)) new-val)))
           (pure (cdr postfixed)))
      (pure (cdr postfixed)))))
  
  
     
  ;can be used in function arguments for function application
  (define argument-position/p
    (binary-op/p single-expression/p))
  ;immediate end
  ;whitespace end
  ;newline end



  ;MAP:
  ;First thing can be:
  ;;;comment - cont but doesn't make function not last
  ;;;function - cont but can't be last
  ;;;identifier -
  ;;;;;;postfixed
  ;;;;;;;;;binary
  ;;;;;;;;;record reassignment
  ;;;;;;reassignment
  ;;;;;;binding
  ;;;;;;binary
  ;;;other primitives -
  ;;;;;postfixed
  ;;;;;;;binary
  ;;;;;;;record reassignment
  ;;;;;binary
  ;;;( 
  ;;;testI
  ;;;testE
  ;;;end but not the first

  (define (continue-block-need-newline end [last-fun? #f])
    (do
        (hidden/p whitespace-no-newline/p)
      (or/p
       (do
           (guard/p (hidden/p end) (lambda (_) (not last-fun?))
                    "a block which ends in a value-returning expression"
                    (lambda (_) "Ended block with a function binding"))
         (pure (list)))
       (do
           (hidden/p (char/p #\newline))
         w-i/p
         (block-position/p end #f last-fun?)))))

  
  (define (class-continue-block-need-newline end [last-fun? #f])
    (do
        (hidden/p whitespace-no-newline/p)
      (or/p
       (do
           (guard/p (hidden/p end) (lambda (_) (not last-fun?))
                    "a block which ends in a value-returning expression"
                    (lambda (_) "Ended block with a function binding"))
         (pure (list)))
       (do
           (hidden/p (char/p #\newline))
         w-i/p
         (class-block-position/p end #f last-fun?)))))

  (define (continue-block-already-newline end)
    (block-position/p end #f))

  (define whitespace-no-newline/p
    (many/p (satisfy/p (lambda (x) (and (char-whitespace? x) (not (eq? x #\newline)))))))

  (define (handle-record-reassign rec-acc end)
    (let ([record (second rec-acc)]
          [field (third rec-acc)])
      (do
          (string/p ":=")
        w-i/p
        [binding <- (syntax/p argument-position/p)]
        [rest <- (syntax/p (continue-block-need-newline end))]
        (pure (cons (list #'#%reassign-field record field binding) rest)))))
  (define (whitespace-prefixed-binary-handler arg end)
    (do
        [op <- (try/p whitespace-prefixed-op/p)]
      [binary <- (syntax/p (binary-op/p single-expression/p
                                        (list arg)
                                        (list op)))]
      [rest <- (syntax/p (continue-block-need-newline end))]
      (pure (cons binary rest))))
  ;Handles Postfix case
  (define (handle-postfix arg end)
    (do
        ;makes sure we fail this case if there is no postfix
        ;collect postfixes does not do this
        [first-postfix <- (syntax/p (postfix/p arg))]
      (let ([syn-list (syntax->list first-postfix)])
        (do
            [postfixed <- (syntax/p (collect-postfixes/p (cdr syn-list) (car syn-list)))]
          (let ([post-list (syntax->list postfixed)])
            ;If the last postfix was record access instead of application,
            ;we handle record reassignment here
            (do
                [spaces <- whitespace-no-newline/p]
              ;CASES:
              ;;;newline
              ;;;;;binary
              ;;;;;reassignment
              ;;;;;plain access
              ;;;plain-access
              ;;;binary
              ;;;reassignment
              (or/p
               (do
                   (char/p #\newline)
                 w-i/p
                 (or/p
                  ;reassignment case
                  (if-def-parse (syntax->datum (car post-list)) (handle-record-reassign (cdr post-list) end))
                  ;binary case
                  (whitespace-prefixed-binary-handler (cdr post-list) end)
                  ;block continues, but we've seen a newline
                  (do
                      [rest <- (syntax/p (continue-block-already-newline end))]
                    (pure (cons (cdr post-list) rest)))))
               ;reassign case
               (if-def-parse (syntax->datum (car post-list)) (handle-record-reassign (cdr post-list) end))
               ;binary case
               (if-def-parse (cons? spaces) (whitespace-prefixed-binary-handler (cdr post-list) end))
               (do
                   [rest <- (syntax/p (continue-block-need-newline end))]
                 (pure (cons (cdr post-list) rest))))))))))
  (define (handle-reassign id end)
    (do
        (string/p ":=")
      w-i/p
      [binding <- (syntax/p argument-position/p)]
      [rest <- (syntax/p (continue-block-need-newline end))]
      (pure (cons (list #'#%reassign id binding) rest))))

  (define handle-equal-signs
    (do
        [equal-signs <- (many/p (char/p #\=) #:min 1)]
      (if-def-parse (= (length equal-signs) 1)
                    (pure #t))))

  (define (handle-bind id end)
    (do
        (try/p handle-equal-signs)
      w-i/p
      [binding <- (syntax/p argument-position/p)]
      (label/p "a block that does not end in a variable binding" at-least-one-newline/p)
      w-i/p
      [rest <- (syntax/p (block-position/p end))]
      (pure (list (append (list #'#%let (list (list id binding))) rest)))))

  
  (define (typed-bind id end)
    (do
        (string/p "::")
      w-i/p
      [type <- (syntax/p (type/p))]
      w-i/p
      (string/p "=")
      w-i/p
      [binding <- (syntax/p argument-position/p)]
      w-i/p
      [rest <- (syntax/p (block-position/p end))]
      (pure (list (append (list #'#%typed-let (list id type binding)) rest)))))
  
  (define (handle-prim prim end id?)
    (or/p
     ;postfix case
     (handle-postfix prim end)
     (do
         [spaces <- whitespace-no-newline/p]
       [newline <- (many/p (char/p #\newline))]
       w-i/p
       (or/p
        ;Handles reassignment
        (if-def-parse (and id? ML-reassign) (try/p (handle-reassign (strip-id prim) end)))
        ;handles reassignment of struct vars
        (if-def-parse (and id? ML-reassign) (typed-bind (strip-id prim) end))
        ;handles binding (typed and untyped)
        (if-def-parse (and id? ML-typed-let) (try/p (typed-bind (strip-id prim) end)))
        (if-def-parse (and id? (not ML-typed-let)) (handle-bind (strip-id prim) end))
        ;handles binary
        (if-def-parse (or (cons? spaces) (cons? newline)) (whitespace-prefixed-binary-handler prim end))
        ;handles plain id
        (if (cons? newline)
            (do [rest <- (syntax/p (continue-block-already-newline end))]
              (pure (cons prim rest)))
            (do [rest <- (syntax/p (continue-block-need-newline end))]
              (pure (cons prim rest))))))))

   (define (class-block-position/p end [empty? #t] [last-fun? #f])
    (or/p
     ;Handles comment case (at least for beginning of lines that aren't the last)
     (do (hidden/p comment/p)
       w-i/p
       (class-block-position/p end empty? last-fun?))
     ;handles method
     (do
         [method <- (syntax/p (method/p))]
       [rest <- (syntax/p (class-continue-block-need-newline end #t))]
       (pure (cons method rest)))
     ;handles field
     (do
         [field <- (syntax/p (field/p))]
       [rest <- (syntax/p (class-continue-block-need-newline end #t))]
       (pure (cons field rest)))
     (do
         end
       (pure (list)))))
  
  (define (block-position/p end [empty? #t] [last-fun? #f])
    (or/p
     ;Handles comment case (at least for beginning of lines that aren't the last)
     (do (hidden/p comment/p)
       w-i/p
       (block-position/p end empty? last-fun?))
     ;Handles the ( case for starting binaries
     (do (char/p #\()
       [binary <- (syntax/p (binary-op/p single-expression/p (list) (list "(")))]
       [postfixed <- (handle-prim binary end #f)]
       (pure postfixed))
     ;Handles the struct identifier case
     #|(do
         [id <- (if-def-parse ML-struct (try/p (syntax/p struct-identifier/p)))]
       (syntax/p (handle-prim id end #t)))|#
     ;Handles the identifier case
     (do
         [id <- (syntax/p identifier/p)]
       (syntax/p (handle-prim id end #t)))
     ;handles the prim case
     (do
         [prim <- (syntax/p primitive/p)]
       (syntax/p (handle-prim prim end #f)))
     ;handles functions
     (if-def-parse
      ML-func
      (do
          [fun <- (syntax/p (function/p))]
        
        ;Options: Continue on
        [rest <- (syntax/p (continue-block-need-newline end #t))]
        (if (null? (syntax->list rest))
            (fail/p (message (srcloc #f #f #f #f #f) "A block ending in a function definition." '("a block ending in an value-returning expression")))
        (pure (cons fun rest)))))
     ;handles typed-functions
     (if-def-parse
      ML-typed-func
      (do
          [fun <- (syntax/p (typed-function/p))]
        
        ;Options: Continue on
        [rest <- (syntax/p (continue-block-need-newline end #t))]
        (if (null? (syntax->list rest))
            (fail/p (message (srcloc #f #f #f #f #f) "A block ending in a function definition." '("a block ending in an value-returning expression")))
        (pure (cons fun rest)))))

     ;handles class definitions
     (if-def-parse
      ML-object
      (do
          [class <- (syntax/p (class/p))]
        
        ;Options: Continue on
        [rest <- (syntax/p (continue-block-need-newline end #t))]
        (if (null? (syntax->list rest))
            (fail/p (message (srcloc #f #f #f #f #f) "A block ending in a class definition." '("a block ending in an value-returning expression")))
        (pure (cons class rest)))))

     ;handles structs
     (if-def-parse
      ML-struct
      (do
          [struct <- (syntax/p (struct/p))]
        
        ;Options: Continue on
        [rest <- (syntax/p (continue-block-need-newline end #t))]
        (if (null? (syntax->list rest))
            (fail/p (message (srcloc #f #f #f #f #f) "A block ending in a function definition." '("a block ending in an value-returning expression")))
        (pure (cons struct rest)))))
     
     (if-def-parse
      ML-typed-let
      (do
          (try/p (hidden/p  (string/p "~testI")))
        [args <- (syntax/p (error-sequence/p argument-position/p))]
        [rest <- (syntax/p (continue-block-need-newline end))]
        (pure (cons (cons #'#%testI-typed args) rest))))

     (if-def-parse
      (not ML-typed-let)
      (do
          (try/p (hidden/p  (string/p "~testI")))
        [args <- (syntax/p (error-sequence/p argument-position/p))]
        [rest <- (syntax/p (continue-block-need-newline end))]
        (pure (cons (cons #'#%testI args) rest))))
     
     (if-def-parse
      ML-typed-let
      (do
          (try/p (hidden/p (string/p "~testE")))
        [args <- (syntax/p (error-sequence/p argument-position/p))]
        [rest <- (syntax/p (continue-block-need-newline end))]
        (pure (cons (cons #'#%testE-typed args) rest))))

     (if-def-parse
      (not ML-typed-let)
      (do
          (try/p (hidden/p (string/p "~testE")))
        [args <- (syntax/p (error-sequence/p argument-position/p))]
        [rest <- (syntax/p (continue-block-need-newline end))]
        (pure (cons (cons #'#%testE args) rest))))
     (do
         end
       (if empty?
           (fail/p (message (srcloc #f #f #f #f #f) "An empty block." '("a block ending in an value-returning expression")))
           (pure (list))))))

  ;Success:
  ;Blank number
  ;Number plus space
  ;Number with all forms of end
  ;prim + (
         
         
  ;the try/p on the fun makes all later options appear too
  ;this is bad.
  ;It should commit after seeing fun.
  (with-syntax ([str (port->string in)])
    ;Note this will allow for error highlighting unless the user puts multiple spaces between the #lang and the lang specification
    (let* ([syntax-start-position (+ 1 (string-length (string-append "#lang " (symbol->string language))))]
           [res (parse-syntax-string  (syntax/p (top-level-block/p))
                                      (datum->syntax #f (syntax->datum #'str) (list src 1 (- syntax-start-position 1) syntax-start-position 0)))])
      (let ([code (parse-result! res)]
            [expander (void)])
        ;In all weirdness, It looks like module expands it's first body before passing it to module begin
        ;(probably to see if it's a require or provide). I've put a void there to make it go away.
        (with-syntax ([(code ...) code]
                      [language language]
                      [expander expander]
                      [module-name module-name])
          (define mod-name-counter 0)
          ;(print #'(code ...))
          (if runAll
              (map (lambda (x) (begin (set! mod-name-counter (+ 1 mod-name-counter))
                                      (if (or ML-typed-let ML-typed-func)
                                          (let ([to-print
                                                 (strip-context #`(module
                                                                      #,(string->symbol
                                                                         (~a mod-name-counter))
                                                                    #,x expander (#%type-check code ...)))])
                                            ;(print to-print)
                                            to-print)
                                          (let ([to-print
                                                 (strip-context #`(module
                                                                      #,(string->symbol
                                                                         (~a mod-name-counter))
                                                                    #,x expander code ...))])
                                            ;(print to-print)
                                            to-print)
                                          )))
                   runAll)
              (if (or ML-typed-let ML-typed-func)
                  (strip-context #`(expander (#%type-check code ...)))
                  (strip-context #`(expander code ...)))))))))