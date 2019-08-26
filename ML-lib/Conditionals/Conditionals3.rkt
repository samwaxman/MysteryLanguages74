#lang racket
(require (prefix-in #% "../Numbers/Numbers1.rkt")
         "../ML-Helpers.rkt"
         (prefix-in #% "ConditionalsSetup.rkt"))
(provide
 (unprefix-out #% "../Numbers/Numbers1.rkt")               
 (all-from-out "ConditionalsSetup.rkt")
 (rename-out [my-if #%if])
 true
 false)

;Only accepts boolean conditions (or things that evaluate
;to booleans)
(define-syntax-rule (my-if cond then else)
  (let ([eval-cond cond])
    (if (boolean? eval-cond) (if eval-cond then else)
             (raise-user-error "The condition for an \"if\" expression must be a boolean."))))
