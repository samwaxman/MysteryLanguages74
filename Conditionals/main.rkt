#lang racket
(require "../ML-lib/ML-Helpers.rkt")
(provide #%module-begin #%datum #%top #%app)

(setup-all-reader (Conditionals1 Conditionals2 Conditionals3)
                  ("------Core 1------" "------Core 2------" "------Core 3------")
                  #:let #:+ #:- #:* #:/
                  #:> #:< #:>= #:<= #:== #:!= #:++
                  #:if)