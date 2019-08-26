#lang racket
(require "../ML-lib/ML-Helpers.rkt")
(provide #%module-begin #%datum #%top #%app)

(setup-all-reader (TypeSystem1 TypeSystem2 TypeSystem3 TypeSystem4)
                  ("------Core 1------" "------Core 2------" "------Core 3------" "------Advanced 1------")
                  #:let #:typed-let #:+ #:- #:* #:/
                  #:> #:< #:>= #:<= #:== #:!= #:++
                  #:if #:typed-func #:struct)