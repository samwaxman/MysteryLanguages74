#lang racket
(require "../ML-Helpers.rkt")
(require "TypeSystemHelpers.rkt")
(require (prefix-in CO- "../Conditionals/Conditionals1.rkt"))
(require test-engine/racket-tests)
(provide
 #%typed-let
 #%typed-func
 #%struct
 #%field-id
 #%type-check
 #%reassign-field
 #%testI-typed
 #%testE-typed
 #%++ #%+ #%- #%/ #%* #%== #%<= #%>= #%< #%>
 (except-out
  (unprefix-out CO- "../Conditionals/Conditionals1.rkt")
   CO-#%++ CO-#%+ CO-#%- CO-#%/ CO-#%* CO-#%== CO-#%<= CO-#%>= CO-#%< CO-#%>))

(define (can-convert? type1 type2)
  (or
   (and (equal? type1 'String) (equal? type2 'Number))
   (and (equal? type1 'Number) (equal? type2 'String))))

(define (same-type? t1 t2 instance-types)
  (or (can-convert? t1 t2) (default-same-type? t1 t2 instance-types)))

(define (full-type-check ast)
  (full-type-checker ast same-type? default-same-type?no-convert))

(define-syntax-rule (#%type-check code ...)
  (begin
    (full-type-check '(code ...))
    code ...))

(define-syntax-rule (#%testI-typed exp args)
  (CO-#%testI exp (#%type-check args)))

(define-syntax-rule (#%testE-typed exp args)
  (CO-#%testE exp (#%type-check args)))

;; ERROR STRINGS ;;
(define ERROR-STRING:cannot-convert "Tried to convert ~s into a ~a but failed.")
;; ERROR STRINGS ;;

(define (unconvertable value new-type)
  (raise-error (format ERROR-STRING:cannot-convert value new-type)))


(define (ML-string->number s)
  (define v (string->number s))
  (cond
    [(boolean? v) (unconvertable s 'Number)]
    [else v]))
(define (ML-number->string n)
  (define v (number->string n))
  (cond
    [(boolean? v) (unconvertable n 'Number)]
    [else v]))

(define (get-convert-type val)
  (cond
    [(string? val) 'String]
    [(number? val) 'Number]
    [else 'Non-Convertable]))

(define (convert val type2)
  (define type1 (get-convert-type val))
  (match (list type1 type2)
    [(list 'String 'Number) (ML-string->number val)]
    [(list 'Number 'String) (ML-number->string val)]
    [else val]))

(define-syntax-rule (#%typed-let (name type bind) body ...)
  (let ([name (convert bind (quote type))]) body ...))

(define-syntax-rule (#%typed-func name ((arg type) ...) return-type body ... lastBody)
  (define (name arg ...)
    (set! arg (convert arg (quote type))) ...
    (check-decrement-fuel)
    body ... (convert lastBody (quote return-type))))

(define-syntax-rule (#%struct struct-name ((e-name type) ...))
  (begin
    
    (define symbols (list (quote e-name) ...))
    (define sym-name (quote struct-name))
    
    (define (struct-name e-name ...) ;; the constructor
      (set! e-name (convert e-name (quote type))) ...
      (define vals (list e-name ...)) ;; the values given to the 'struct-name' constructor
      (define h (make-hash (map cons symbols vals)))
      (define len (hash-count h))
      
      (define (get-func e) (hash-ref h e))
      (define (set-func e v) (hash-set! h e (convert v (get-convert-type e))))
      
      (ml-struct sym-name get-func set-func vals (next-global-id))
      )))

(#%typed-func add ((a Number) (b Number)) Number (+ a b))
(#%typed-func subtract ((a Number) (b Number)) Number (- a b))
(#%typed-func divide ((a Number) (b Number)) Number
  (cond
    [(and (eq? 0 a) (eq? 0 b)) +nan.0]
    [(eq? 0 b) (* +inf.0 (sgn a))]
    [(nan? b) +nan.0]
    [else (/ a b)]))
(#%typed-func multiply ((a Number) (b Number)) Number
  (cond
    [(or (nan? a) (nan? b)) +nan.0]
    [(and (= 0 a) (infinite? b)) +nan.0]
    [(and (= 0 b) (infinite? a)) +nan.0]
    [else (* a b)]))


(#%typed-func ML== ((a Number) (b Number)) Boolean (= a b))
(#%typed-func ML<= ((a Number) (b Number)) Boolean (<= a b))
(#%typed-func ML>= ((a Number) (b Number)) Boolean (>= a b))
(#%typed-func ML< ((a Number) (b Number)) Boolean (< a b))
(#%typed-func ML> ((a Number) (b Number)) Boolean (> a b))

(#%typed-func ML-string-append ((s1 String) (s2 String)) String (string-append s1 s2))

(define-syntax-rule (#%++ str1 str2) (ML-string-append str1 str2))

(define-syntax-rule (#%+ val1 val2) (add val1 val2))
(define-syntax-rule (#%- val1 val2) (subtract val1 val2))
(define-syntax-rule (#%/ val1 val2) (divide val1 val2))
(define-syntax-rule (#%* val1 val2) (multiply val1 val2))

(define-syntax-rule (#%== val1 val2) (ML== val1 val2))
(define-syntax-rule (#%<= val1 val2) (ML<= val1 val2))
(define-syntax-rule (#%>= val1 val2) (ML>= val1 val2))
(define-syntax-rule (#%< val1 val2) (ML< val1 val2))
(define-syntax-rule (#%> val1 val2) (ML> val1 val2))