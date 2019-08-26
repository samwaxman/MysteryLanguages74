#lang racket
(require
  (only-in
   racket [+ r+] [- r-] [* r*] [/ r/] [let r-let])
  "../ML-Helpers.rkt"
  "NumbersSetup.rkt"
  "../tester.rkt"
  (for-syntax syntax/parse))
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

;Note: don't build languages on top of this
;Overflow is being called in all places a number can be introduced
;(the user types it, or it was the result of arithmetic)
;it wouldn't proc on things like the return value of functions


;THIS LANGUAGE MAKES DECIMAL NUMBERS INEXACT
;AND INTEGERS EXACT

;;Making numbers that act like 64 bit integers

;These are the maximum and minimum representable integers
;using 64 bits
(define int-max 9223372036854775807)
(define int-min -9223372036854775808)
;This is the total number of integers we can represent in 64 bits
(define num-total (r+ (r- int-max int-min) 1))

;This function implements wrapping. If a number overflows, or goes
;over the max int/under the min int, it moves it back between them
;in the way that a regular 64 bit int would do this.
(define (overflow num)
  (if (exact-integer? num)
      (let ([flow-num (modulo num num-total)])
        (if (<= flow-num int-max) flow-num (r- flow-num num-total)))
      num))


;;Not a need to use overflow. If we use quotient, the result is smaller than either number in
;;absolute value, and thus cannot be over max or under min. If the result is the regular divide,
;;we're using floats, and we don't need to overflow.
(define (divide a b)
  (if (and (exact-integer? a) (exact-integer? b))
      (if (= b 0)
          ;Cant divide by 0 if using ints
          (raise-user-error "Cannot divide by zero.")
          (quotient a b))
      ;Weirdly enough, 1.0 / 0 in racket is an error.
      ;The floating point contagion doesn't seem to come
      ;before the 0 is checked
      (cond
        [(and (= 0 a) (= 0 b)) +nan.0]
        [(nan? b) +nan.0]
        [(= 0 b) (r/ a 0.0)]
        [else (r/ a b)]
        )))
(define (add a b)
  (overflow (r+ a b)))
(define (subtract a b)
  (overflow (r- a b)))
(define (multiply a b)
  (cond
    [(or (nan? a) (nan? b)) +nan.0]
    [(and (= 0 a) (infinite? b)) +nan.0]
    [(and (= 0 b) (infinite? a)) +nan.0]
    [else (overflow (r* a b))]))


;The reader reads in numbers as strings wrapped in this function.
;It's up to the language implementation to then turn the strings into the
;proper numbers.
(define (#%number num-string)
  (cond
    [(equal? num-string "Inf") +inf.0]
    [(equal? num-string "-Inf") -inf.0]
    [(equal? num-string "NaN") +nan.0]
    [else (let ([num
           (string->number num-string)])
            ;important we use equal? not = or 0 would become 0.0
            (overflow (if (equal? num -0.0) 0.0 num)))]))

(define-syntax-rule (my-mod body ...)
  (parameterized-mod-begin
   add subtract multiply divide body ...))