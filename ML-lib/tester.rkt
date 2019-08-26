#lang racket
(provide #%testI
         test-num
         test-failed
         inc-test-num
         inc-test-failed
         recap
         #%testE
         #%testSynE
         side-effect)

(require "ML-Helpers.rkt")
(require syntax/macro-testing)
(define test-num 1)
(define test-failed 0)
(define (inc-test-num) (set! test-num (+ 1 test-num)))
(define (inc-test-failed) (set! test-failed (+ 1 test-failed)))

;Testing infrastructure


;Tests two strings for equality. If they match, passes the test,
;if not, fails.
(define (test-string-equality expected produced)
  (if (string=? expected produced)
      (display (~a "Test " test-num " passed.\n"))
      (begin (display (~a "Test " test-num " failed. Was expecting " expected " but got " produced ".\n"))
             (inc-test-failed)))
  (inc-test-num))

;Runs the given non-empty program and compares its printed
;result with expected
(define-syntax-rule (#%testI expected prog-first program ...)
  (let ()
    (define string-to-check "")
    (parameterize ([current-output-port (open-output-string)])
      (let ([produced (let () prog-first program ...)])
        (set! string-to-check (string-append (get-output-string (current-output-port)) (~my-s produced)))))
    (test-string-equality expected string-to-check)))

;Displays how many tests passed out of the total number of tests.
(define (recap)
  (display (~a "\n" (- test-num test-failed 1) '/ (- test-num 1) " passed.")))


;TestE runs the given non-empty program and checks its error message
;against the expected message 
(define prompt-tag (make-continuation-prompt-tag))

(define-syntax-rule (#%testE check-against prog-first prog ...)
  (call-with-continuation-prompt
   (lambda ()
     (call-with-exception-handler
      (lambda (x) (abort-current-continuation prompt-tag x))
      (lambda () prog-first prog ...
        (begin (display (~a "Test " test-num " failed. Was expecting an error with message \"" check-against "\" but program did not throw an error.\n")) (inc-test-failed) (inc-test-num)))))
   prompt-tag
   (lambda (v)
     (test-string-equality
      check-against
      (vector-ref (struct->vector v) 1)))))


;Not in current use. Catches syntax errors and
;checks their error messages against check-against.
(define-syntax (#%testSynE stx)
  (syntax-case stx ()
    [(_ check-against prog-first prog ...)
     #'(call-with-continuation-prompt
        (lambda ()
          (call-with-exception-handler
           (lambda (x) (abort-current-continuation prompt-tag x))
           (lambda () (convert-compile-time-error (begin prog-first prog ...)))))
        prompt-tag (lambda (v)
                     (test-string-equality
                      check-against
                      (vector-ref (struct->vector v) 1))))]))



;;Good for testing if arguments to macros get evaluated only once
;;Probably want to make one to test that things get evaluated
;;in the proper order as well.
(define (side-effect num)
  (begin
    (printf "|")
    num))
