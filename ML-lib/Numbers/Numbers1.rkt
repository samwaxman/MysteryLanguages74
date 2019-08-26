#lang racket
(require
  (rename-in
   racket [+ r+] [- r-] [* r*] [/ r/] [let r-let])
  "../ML-Helpers.rkt"
  "NumbersSetup.rkt"
  "../tester.rkt")
(provide
 (rename-out [ML-let #%let] [ML+ #%+] [ML- #%-][ML* #%*] [ML/ #%/]
             ;Arity app is defined in ML-Helpers
             ;It display better arity error messages
             [#%arity-app #%app]
             [my-mod #%module-begin]
             [my-top #%top]
             [student-print print])
 #%datum
 #%id
 #%block
 #%number
 (all-from-out "../tester.rkt"))


;THIS LANGUAGE MAKES ALL NUMBERS EXACT


(define (divide a b)
  (cond
    [(and (eq? 0 a) (eq? 0 b)) +nan.0]
    [(eq? 0 b) (* +inf.0 (sgn a))]
    [(nan? b) +nan.0]
    [else (r/ a b)]))
(define (add a b)
  (r+ a b))
(define (subtract a b)
  (r- a b))
(define (multiply a b)
  (cond
    [(or (nan? a) (nan? b)) +nan.0]
    [(and (= 0 a) (infinite? b)) +nan.0]
    [(and (= 0 b) (infinite? a)) +nan.0]
    [else (r* a b)]))

(define-syntax-rule (my-mod body ...)
  (parameterized-mod-begin
   add subtract multiply divide body ...))

;The reader reads in numbers as strings wrapped in this function.
;It's up to the language implementation to then turn the strings into the
;proper numbers.

(define (#%number num-string)
  (cond
    [(equal? num-string "Inf") +inf.0]
    [(equal? num-string "-Inf") -inf.0]
    [(equal? num-string "NaN") +nan.0]
    ;#e is racket's prefix for exact numbers. Stored precisely
    [else (string->number (string-append "#e" num-string))]))


