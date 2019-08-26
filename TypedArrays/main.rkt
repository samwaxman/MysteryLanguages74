#lang racket
(require "../ML-lib/ML-Helpers.rkt")
(provide #%module-begin #%datum #%top #%app)

(setup-all-reader (TypedArrays1 TypedArrays2 TypedArrays3 TypedArrays4 TypedArrays5)
                  ("------Core 1------" "------Core 2------" "------Core 3------" "------Core 4------" "------Core 5------")
                  #:let #:typed-let #:+ #:- #:* #:/
                  #:> #:< #:>= #:<= #:== #:!= #:++
                  #:if #:typed-func #:struct #:typed-arrays
                  #:object #:reassign)