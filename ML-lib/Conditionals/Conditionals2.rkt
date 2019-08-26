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
;Providing racket's if would yield different error messages.
;In particular, (if 1 2) says "missing else expression"
;Doesn't matter because the reader won't let it happen,
;but it's best to be safe.

;If in this language treats everything as truthy but false
(define-syntax-rule (my-if cond then else)
  (if cond then else))

