#lang racket
(require "../ML-lib/ML-Helpers.rkt")
(provide #%module-begin #%datum #%top #%app)

(setup-all-reader (MutableStructures1 MutableStructures2 MutableStructures3)
                  ("------Core 1------" "------Core 2------" "------Advanced 1------")
                  #:let #:+ #:- #:* #:/
                  #:> #:< #:>= #:<= #:== #:!= #:++
                  #:if #:record #:lambda #:func #:reassign)