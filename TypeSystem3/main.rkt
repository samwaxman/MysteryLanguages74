#lang racket
;This prefix in is so that this file doesn't have the
;notions of module-begin, app, etc defined in numbers
(require (prefix-in #% "../ML-lib/TypeSystem/TypeSystem3.rkt")
         "../ML-lib/ML-Helpers.rkt")
(provide (unprefix-out #% "../ML-lib/TypeSystem/TypeSystem3.rkt"))


(setup-reader TypeSystem3
              #:let #:typed-let #:+ #:- #:* #:/
              #:> #:< #:>= #:<= #:== #:!= #:++
              #:if #:typed-func #:struct)