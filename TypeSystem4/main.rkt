#lang racket
(require (prefix-in #% "../ML-lib/TypeSystem/TypeSystem4.rkt")
         "../ML-lib/ML-Helpers.rkt")
(provide (unprefix-out #% "../ML-lib/TypeSystem/TypeSystem4.rkt"))


(setup-reader TypeSystem4
              #:let #:typed-let #:+ #:- #:* #:/
              #:> #:< #:>= #:<= #:== #:!= #:++
              #:if #:typed-func #:struct)