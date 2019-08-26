#lang racket
(require syntax/parse
         racket/splicing
         (for-syntax syntax/parse
                     racket/base
                     racket/format)
         (except-in racket + - / * let)
         (rename-in racket [+ r+] [- r-] [* r*] [/ r/] [let r-let])
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


(define (divide a b)
  (r/ a b))
(define (add a b)
  (r+ a b))
(define (subtract a b)
  (r- a b))
(define (multiply a b)
  
  (r* a b))


(define-syntax-rule (my-mod body ...)
  (parameterized-mod-begin
   add subtract multiply divide body ...))


;The reader reads in numbers as strings wrapped in this function.
;It's up to the language implementation to then turn the strings into the
;proper numbers.

;Note, we do exact->inexact rather than prepending
;an #i because if we just do the latter, -0
;will become -0.0 which is distinguishable from
;the exact language, in which -0 becomes regular 0
(define (#%number num-string)
  (cond
    [(equal? num-string "Inf") +inf.0]
    [(equal? num-string "-Inf") -inf.0]
    [(equal? num-string "NaN") +nan.0]
    [else (exact->inexact
           (string->number (string-append "#e" num-string)))]))
