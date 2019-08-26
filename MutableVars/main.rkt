#lang racket
(require "../ML-lib/ML-Helpers.rkt")
(provide #%module-begin #%datum #%top #%app)

(setup-all-reader (MutableVars1 MutableVars2 MutableVars3)
                  ("------Core 1------" "------Core 2------" "------Advanced 1------")
                  #:let #:+ #:- #:* #:/
                  #:> #:< #:>= #:<= #:== #:!= #:++
                  #:if #:func #:lambda #:reassign)