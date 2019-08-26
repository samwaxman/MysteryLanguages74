#lang racket
(require (prefix-in ML- "../AnonymousFunctions/AnonymousFunctions1.rkt")
         "../NamedFunctions/NamedFunctions1.rkt"
         "../ML-Helpers.rkt"
         "ForHelpers.rkt")
(provide
 (unprefix-out ML- "../AnonymousFunctions/AnonymousFunctions1.rkt")
 #%for
 #%func
 first
 rest
 link
 is-empty
 is-link
 (rename-out [list #%list]
             [null empty]
             [student-print print]))

;This for has a fresh variable for each run
(define-syntax-rule (#%for id list body ...)
  (let ([eval-list list])
    (if (list? eval-list)
        (map (lambda (id) body ...) eval-list)
        (raise-user-error (~a "for expected a list to iterate through but received " (~my-s eval-list))))))


