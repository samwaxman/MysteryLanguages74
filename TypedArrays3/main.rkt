#lang racket
;This prefix in is so that this file doesn't have the
;notions of module-begin, app, etc defined in numbers
(require (prefix-in #% "../ML-lib/TypedArrays/TypedArrays3.rkt")
         "../ML-lib/ML-Helpers.rkt")
(provide (unprefix-out #% "../ML-lib/TypedArrays/TypedArrays3.rkt"))


(setup-reader TypedArrays3
              #:let #:typed-let #:+ #:- #:* #:/
              #:> #:< #:>= #:<= #:== #:!= #:++
              #:if #:typed-func #:struct #:typed-arrays
              #:object #:reassign)