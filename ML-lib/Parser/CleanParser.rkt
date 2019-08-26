#lang racket

(require megaparsack megaparsack/text)
(require data/monad)
(require data/applicative)
(require (for-syntax syntax/parse))
(require syntax/strip-context)

(provide ML-parse)

(define (string-in/p list)
    (if (cons? list)
        (apply or/p (map (lambda (x) (try/p (string/p x))) list))
        (fail/p (message (srcloc #f #f #f #f #f) "Parsing rule for binary-operations must be turned off if no binary operations"
                         '()))))

  (define-syntax-rule (parse-if cond parser)
    (do (if cond parser (fail/p (message (srcloc #f #f #f #f #f) #f '())))))

  ;Won't consume the last seperator or commit on separators
  ;followed by non-patterns
  (define (non-committing-many/p parser seperator [max +inf.0])
    (define (helper max)
      (if (= max 0)
          (pure (list))
          (do
              [parsed <- parser]
            [rest <- (many/p (try/p (do seperator parser)) #:max (- max 1))]
            (pure (cons parsed rest)))))
    (or/p
     (helper max)
     (pure (list))))

  (define (default-stringify list)
    (foldl (lambda (x a) (~a a x)) "" list))
  (define d default-stringify)

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
 (define acceptable-identifier-chars
    (list->string
     (list #\_ #\- #\+ #\\ #\*)))


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
                  #:arrays [ML-arrays #f])

  
  


  (define keywords
    (let ()
      (define all-keywords (list "block" "end" "Inf" "-Inf" "NaN"))
      (when ML-if (set! all-keywords (append (list "if" "true" "else" "false") all-keywords)))
      (when ML-for (set! all-keywords (append (list "for" "in") all-keywords)))
      (when ML-lambda (set! all-keywords (cons "lam" all-keywords)))
      (when ML-func (set! all-keywords (cons "fun" all-keywords)))
      all-keywords))

  (define binary-ops
    (let ()
      (define all-binary '())
      (when ML+ (set! all-binary (cons "+" all-binary)))
      (when ML- (set! all-binary (cons "-" all-binary)))
      (when ML* (set! all-binary (cons "*" all-binary)))
      (when ML/ (set! all-binary (cons "/" all-binary)))
      (when ML++ (set! all-binary (cons "++" all-binary)))
      (when ML== (set! all-binary (cons "==" all-binary)))
      (when ML!= (set! all-binary (cons "!=" all-binary)))
      (when ML< (set! all-binary (cons "<" all-binary)))
      (when ML<= (set! all-binary (cons "<=" all-binary)))
      (when ML> (set! all-binary (cons ">" all-binary)))
      (when ML>= (set! all-binary (cons ">=" all-binary)))
      all-binary))

 

  ;;If you do end = 3, you don't see identifier on the list
  ;or get the error message because of the try and the label
  ;could move guard to expr, but a lil much
  (define-syntax-rule (raw-identifier/p)
    (label/p "identifier" (guard/p
                           (or/p
                            (do
                                [start <- letter/p]
                              [continue <- (label/p "identifier char" (many/p (or/p (char-in/p acceptable-identifier-chars)
                                                                                    letter/p
                                                                                    digit/p)))]
                              (pure (string->symbol (d (list start (d continue)))))) 
                            (do
                                [start <- (char-in/p acceptable-identifier-chars)]
                              [continue <- (label/p "identifier char" (many/p (or/p (char-in/p acceptable-identifier-chars)
                                                                                    letter/p
                                                                                    digit/p)))]
                              (pure (string->symbol (d (list start (d continue)))))))
                           (lambda (id) (not (or (member (symbol->string id) binary-ops)
                                                 (member (symbol->string id) keywords)
                                                 (string->number (symbol->string id)))))
                           #f
                           (lambda (id) (~a "Cannot use a keyword or binary operation as an identifier: " id)))))

  (define-syntax-rule (identifier/p)
    (do
        [id <- (syntax/p (raw-identifier/p))]
      (pure (list #'#%id id))))


  ;NUMBER PARSING
  (define-syntax-rule (number/p)
    (or/p
     (do
        (string/p "NaN")
      (pure (list #'#%number "NaN")))
     (do
         [sign <- (many/p (char/p #\-) #:max 1)]
       (or/p
        (do
            (string/p "Inf")
          (if (null? sign)
              (pure (list #'#%number "Inf"))
              (pure (list #'#%number "-Inf"))))
        (do
            [int <- integer/p]
          [decimal <- (many/p (do
                                  (char/p #\.)
                                [deci <- (many/p digit/p #:min 1)]
                                (pure (~a "." (d deci)))) #:max 1)]
          (pure (list #'#%number (d (list (d sign) int (d decimal))))))))))

  ;;IF PARSING
  (define-syntax-rule (if/p)
    (do
        (string/p "if")
      w-i/p
      [condition <- (syntax/p (argument-position/p))]
      w-i/p
      (char/p #\:)
      w-i/p
      [then <- (syntax/p (argument-position/p))]
      w-i/p
      (string/p "else")
      w-i/p
      (char/p #\:)
      w-i/p
      [else <- (syntax/p (argument-position/p))]
      w-i/p
      (string/p "end")
      (pure (list #'#%if condition then else))))

  ;STRING PARSING
  (define-syntax-rule (special-chars/p)
    (do
        (string/p "\\\"")
      (pure #\")))
  (define-syntax-rule (regular-chars/p)
    (or/p
     (char-between/p #\space #\!)
     (char-between/p #\# #\[)
     (char-between/p #\] #\~)))
  (define string-inside/p
    (label/p
     "a text char or a finishing quote"
     (or/p
      (do
          (string/p "\"")
        (pure (list)))
      (do
          [chars <- (or/p (regular-chars/p) (special-chars/p))]
        [rest <- string-inside/p]
        (pure (cons chars rest))))))

  (define-syntax-rule (user-string/p)
    (do
        (string/p "\"")
      [text <- string-inside/p]
      (pure (list->string text))))

  ;;FUNCTION PARSING
  (define function/p
    (do
        (string/p "fun")
      w/p
      [fun-name <- (syntax/p (raw-identifier/p))]
      (char/p #\()
      w-i/p
      [fun-arguments <- (many/p (syntax/p (raw-identifier/p)) #:sep (do w-i/p (char/p #\,) w-i/p))]
      w-i/p
      (char/p #\))
      w-i/p
      (char/p #\:)
      w-i/p
      [block <- (syntax/p block/p)]
      (pure (append (list #'#%func fun-name fun-arguments) block))))

  ;;REASSIGNMENT PARSING
  (define (reassign-id/p id)
    (do
        [binding <- (syntax/p (argument-position/p))]
      (pure (list #'#%reassign id binding))))

  (define get-reassign-id
    (do
        [id <- (syntax/p (raw-identifier/p))]
      w-i/p
      (string/p ":=")
      (pure id)))

  (define reassign/p
    (do
        [id <- (try/p (syntax/p get-reassign-id))]
      w-i/p
      (reassign-id/p id)))

  ;LAMBDA PARSING

  (define lambda/p
    (do
        (string/p "lam")
      w-i/p
      (char/p #\()
      [args <- (syntax/p (many/p (syntax/p (raw-identifier/p)) #:sep (do w-i/p (char/p #\,) w-i/p)))]
      w-i/p
      (char/p #\))
      w-i/p
      (char/p #\:)
      w-i/p
      [body <- (syntax/p block/p)]
      (pure (append (list #'#%lambda args) body))))

  ;LIST PARSING
  (define (list/p)
    (do
        (char/p #\[)
      w-i/p
      (string/p "list:")
      w-i/p
      [elements <- (many/p (syntax/p (argument-position/p)) #:sep (do w-i/p (char/p #\,) w-i/p))]
      w-i/p
      (char/p #\])
      (pure (cons #'#%list elements))))

  ;FOR PARSING
  (define (for/p)
    (do
        (try/p (string/p "for"))
      w/p
      [var <- (syntax/p (raw-identifier/p))]
      w/p
      (string/p "in")
      w/p
      [list-exp <- (syntax/p (argument-position/p))]
      w-i/p
      (char/p #\:)
      w-i/p
      [block <- (syntax/p (general-block/p (try/p (string/p "end"))))]
      (pure (append (list #'#%for var list-exp) block))))

  ;ARRAY PARSING
  (define-syntax-rule (array/p)
    (do
        (char/p #\[)
      w-i/p
      (string/p "array:")
      w-i/p
      [elements <- (many/p (syntax/p (argument-position/p)) #:sep (do w-i/p (char/p #\,) w-i/p))]
      w-i/p
      (char/p #\])
      (pure (cons #'#%to-array elements))))

  ;RECORD PARSING
  (define-syntax-rule (record/p)
    (do
        (char/p #\{)
      [entries <-
               (many/p
                (do
        [field <- (syntax/p (argument-position/p))]
      w-i/p
      (char/p #\:)
      w-i/p
      [binding <- (syntax/p (argument-position/p))]
      w-i/p
      (pure (list field binding)))
                #:sep (do (char/p #\,) w-i/p))]
      (char/p #\})
      (pure (list #'#%record entries))))

  ;BLOCK PARSING
  (define user-block/p
    (do
        (string/p "block")
      w-i/p
      (char/p #\:)
      w-i/p
      [block <- (syntax/p block/p)]
      (pure (cons #'#%block block))))

  ;POSTFIXES (function application, array access, record access)
  (define (postfix/p arg)
    (or/p
     (parse-if
      ML-record
      (do
          (char/p #\[)
        w-i/p
        [accessor <- (syntax/p (argument-position/p))]
        w-i/p
        (char/p #\])
        (pure (cons #t (list #'#%access arg accessor)))))
     (do
         (char/p #\()
       w-i/p
       [args <- (many/p (syntax/p (argument-position/p)) #:sep (do w-i/p (char/p #\,) w-i/p))]
       w-i/p
       (char/p #\))
       (pure (cons #f (cons arg  args))))
     (parse-if
      ML-arrays
      (do
          (char/p #\[)
        w-i/p
        [index <- (syntax/p (argument-position/p))]
        w-i/p
        (char/p #\])
        (pure (cons #f (list #'#%array-access arg index)))))))
  (define (collect-postfixes/p arg record-a?)
    (or/p
     (do
         [postfixed <- (syntax/p (postfix/p arg))]
       (collect-postfixes/p (cdr (syntax->list postfixed)) (car (syntax->list postfixed))))
     (pure (cons record-a? arg))))
  (define boolean/p
    (label/p "boolean" (do
                           [bool <- (or/p
                                     (string/p "true")
                                     (string/p "false"))]
                         (pure (string->symbol bool)))))
  (define partial-unpostfixed-expression/p
    (or/p
     (parse-if ML-record (label/p "record" (record/p)))
     (parse-if ML-reassign reassign/p)
     ;If you want both lists and arrays, follow reassign format
     ;and make the commit point at list: and array: instead of [
     (parse-if ML-list (label/p "list" list/p))
     (parse-if ML-arrays (label/p "array" (array/p)))
     (label/p "string" (user-string/p))
     (try/p (label/p "identifier" (identifier/p)))
     (try/p (label/p "number" (number/p)))
     (parse-if ML-for for/p)
     (parse-if ML-lambda lambda/p)
     (parse-if ML-if (if/p))
     (parse-if ML-if (try/p boolean/p))
     user-block/p))

  (define (collect-record-access parser)
    (do
        [parsed <- (syntax/p parser)]
      [record-access <- (collect-postfixes/p parsed #f)]
      (parse-if (car record-access)
                (do w-i/p (string/p ":=")
                  (pure record-access)))))

  (define unpostfixed-expression/p
    (or/p
     (do
         [record-access <- (try/p (syntax/p (collect-record-access partial-unpostfixed-expression/p)))]
       (let* ([rec-list (syntax->list record-access)]
              [record (third rec-list)]
              [field (fourth rec-list)])
         (do
             w-i/p
           [binding <- (syntax/p (argument-position/p))]
           (pure (list #'#%reassign-field record field binding)))))
     partial-unpostfixed-expression/p))

  (define expression/p
    (or/p
     (do
         [record-access <- (try/p (syntax/p (collect-record-access unpostfixed-expression/p)))]
       (let* ([rec-list (syntax->list record-access)]
              [record (third rec-list)]
              [field (fourth rec-list)])
         (do
             w-i/p
           [binding <- (syntax/p (argument-position/p))]
           (pure (list #'#%reassign-field record field binding)))))
     (do
         [parsed <- (syntax/p unpostfixed-expression/p)]
       [postfixed <- (collect-postfixes/p parsed #f)]
       (pure (cdr postfixed)))))

  (define operator/p
    (label/p
     "a whitespace surrounded operator"
     (try/p (do
                w/p
              [operator <- (string-in/p binary-ops)]
              w/p
              (pure operator)))))



  (define (count-open-parens paren-list)
    (foldl (lambda (x num) (cond
                             [(equal? x "(") (+ num 1)]
                             [(equal? x ")") (- num 1)]
                             [else num])) 0 paren-list))
  (define-syntax-rule (binary-operations-to-list/p)
    (let ()
      (define (helper so-far)
        (do
            [close-parens <- (non-committing-many/p (string/p ")") w-i/p (count-open-parens so-far))]
          (or/p
           (do
               [operator <- (label/p "a whitespace surrounded operator" (try/p operator/p))]
             [open-parens <- (non-committing-many/p (string/p "(") w-i/p)]
             w-i/p
             [argument <- (syntax/p expression/p)]
             [rest <- (helper (append so-far close-parens (list operator) open-parens (list argument)))]
             (pure rest))
           (pure (append so-far close-parens)))))
  
      (do
          ;using string/p instead of char to make it consistent with operators
          [open-parens <- (hidden/p (non-committing-many/p (string/p "(") w-i/p))]
        w-i/p
        [first <- (syntax/p expression/p)]
        [rest <- (helper (append open-parens (list first)))]
        (pure rest))))


  (define (precedence op)
    (cond
      [(equal? "(" op) -inf.0]
      [(or (equal? "*" op) (equal? "/" op)) 1]
      [(member op binary-ops) 0]))
  (define (binary-ops->postfix binary-list)
    (define (right-paren-helper output operations remaining)
      (cond
        [(null? operations) (raise-user-error "Missing open parenthesis.")]
        [(and (cons? operations) (equal? (car operations) "("))
         (helper output (cdr operations) remaining)]
        [(cons? operations) (right-paren-helper (cons (car operations) output)
                                                (cdr operations) remaining)]))
    (define (helper output operations remaining)
      (cond
        [(null? remaining) (append (reverse operations) output)]
        [(cons? remaining)
         (cond
           [(or (member (car remaining) binary-ops)
                (equal? (car remaining) "("))
            (if (or (null? operations)
                    (equal? (car remaining) "(")
                    (> (precedence (car remaining)) (precedence (car operations))))
                (helper output (cons (car remaining) operations) (cdr remaining))
                (helper (cons (car operations) output) (cdr operations) remaining))]
           [(equal? (car remaining) ")") (right-paren-helper output operations (cdr remaining))]
           [else (helper (cons (car remaining) output) operations (cdr remaining))])]))
    (helper '() '() binary-list))

  (define (to-prefix-notation postfix-list)
    (define (helper h-list)
      (if (member (car h-list)binary-ops)
          (let-values ([(first f-processed) (helper (cdr h-list))])
            (let-values ([(second s-processed) (helper (drop h-list (+ f-processed 1)))])
              (values (reverse (list first second (op->syntax (car h-list)))) (+ 1 f-processed s-processed))))
          (values (car h-list) 1)))
    (let-values ([(prefix _) (helper postfix-list)])
      prefix))

  (define (op->syntax op)
    (datum->syntax #f (string->symbol (string-append "#%" op))))

  (define (operations-list->prefix-notation list)
    (let ([postfix (binary-ops->postfix list)])
      (if (member "(" postfix) (raise-user-error "Missing close parenthesis.")
          (to-prefix-notation postfix))))

  (define-syntax-rule (binary-operations/p)
    (do
        [binary-list <- (binary-operations-to-list/p)]
      (pure (operations-list->prefix-notation binary-list))))

  (define-syntax-rule (argument-position/p)
    (binary-operations/p))
  (define let-commit-point
    (do
        [id <- (label/p "identifier" (syntax/p (raw-identifier/p)))]
      w-i/p
      (char/p #\=)
      [= <- (many/p (char/p #\=))]
      (parse-if (null? =)
                (pure id))))
  (define (let/p end)
    (do
        [id <- (try/p let-commit-point)]
      w-i/p
      [binding <- (syntax/p (argument-position/p))]
      at-least-one-newline/p
      w-i/p
      [bodies <- (general-block/p end)]
      (pure (append (list #'#%let (list (list id binding))) bodies))))

  ;on each new line check if let or not
  ;if let parse all.
  (define block-position/p
    (do
        (or/p
         (argument-position/p)
         function/p
         (hidden/p (try/p (do
                              (string/p "~testI(")
                            w-i/p
                            [args <- (many/p (syntax/p (argument-position/p)) #:sep (do w-i/p (char/p #\,) w-i/p))]
                            w-i/p
                            (char/p #\))
                            (pure (cons #'#%testI args)))))
         (hidden/p (try/p (do
                              (string/p "~testE(")
                            w-i/p
                            [args <- (many/p (syntax/p (argument-position/p)) #:sep (do w-i/p (char/p #\,) w-i/p))]
                            w-i/p
                            (char/p #\))
                            (pure (cons #'#%testE args))))))))

  (define (block-bodies end)
    (or/p
     (do
         [let <- (syntax/p (let/p end))]
       (pure (list let)))
     (do
         [body <- (syntax/p block-position/p)]
       (or/p
        (try/p (do
                   w-i/p
                 end
                 (pure (list body))))
        (do
            at-least-one-newline/p
          w-i/p
          [rest <- (block-bodies end)]
          (pure (cons body rest)))))
     (do
         end
       (pure (list)))))
  (define (general-block/p end)
    (do
        [bodies <- (block-bodies end)]
      (guard/p
       (guard/p (pure bodies)
                (lambda (x) (cons? x))
                "a non empty block"
                (lambda (_) "An empty block"))
       (lambda (x)(let ([last-list (syntax->list (last x))])
                    (or (not last-list) (null? last-list) (not (equal? '#%func (syntax->datum (car last-list)))))))
       "a block whose last expression is not a function definition"
       (lambda (_) "A function definition at the end of a block."))))

  (define block/p
    (general-block/p (try/p (string/p "end"))))
  (define top-level-block/p
    (do
        w-i/p
      (or/p
       (do eof/p
         (pure (list)))
       (general-block/p eof/p))))
  (define (remove-comments string)
    (define (helper mode remaining)
      (cond
        [(null? remaining) '()]
        [(cons? remaining) (cond
                             [(and (equal? mode 'comment) (not (equal? (car remaining) #\newline)))
                              (cons #\space (helper mode (cdr remaining)))]
                             [(equal? mode 'comment)
                              (cons #\newline (helper 'regular (cdr remaining)))]
                             [(equal? (car remaining) #\;)
                              (cons #\space (helper 'comment (cdr remaining)))]
                             [else (cons (car remaining) (helper mode (cdr remaining)))])]))
    (list->string (helper 'regular (string->list string))))

 
    
  (with-syntax ([str (remove-comments (port->string in))])
    ;;Allows for error highlighting
    (let* ([syntax-start-position (+ (if runAll 0 1) (string-length (string-append "#lang " (symbol->string language))))]
           [res (parse-syntax-string  (syntax/p top-level-block/p)
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
                                      (let ([to-print
                                             (strip-context #`(module
                                                                  #,(string->symbol
                                                                     (~a mod-name-counter))
                                                                #,x expander code ...))])
                                        ;(print to-print)
                                        to-print)))
                   runAll)
              (strip-context #`(expander code ...))))))))
