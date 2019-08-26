#lang racket
(require "../ML-lib/ML-Helpers.rkt")
(provide #%module-begin #%datum #%top #%app)

(setup-all-reader (Scope1 Scope2 Scope3 Scope4)
                  ("------Core 1------" "------Core 2------" "------Advanced 1------" "------Advanced 2------")
                  #:let #:+ #:- #:* #:/
                  #:> #:< #:>= #:<= #:== #:!= #:++
                  #:if #:func #:reassign)
