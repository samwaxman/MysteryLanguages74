#lang racket
(require "../ML-lib/ML-Helpers.rkt")
(provide #%module-begin #%datum #%top #%app)

(setup-all-reader (FunctionCalls1 FunctionCalls2 FunctionCalls3)
                  ("------Core 1------" "------Core 2------" "------Core 3------")
                  #:let #:+ #:- #:* #:/
                  #:> #:< #:>= #:<= #:== #:!= #:++
                  #:if #:func #:reassign)