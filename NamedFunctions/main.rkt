#lang racket
(require "../ML-lib/ML-Helpers.rkt")
(provide #%module-begin #%datum #%top #%app)

(setup-all-reader (NamedFunctions1 NamedFunctions2 NamedFunctions3)
                  ("------Core 1------" "------Core 2------" "------Core 3------")
                  #:let #:+ #:- #:* #:/
                  #:> #:< #:>= #:<= #:== #:!= #:++
                  #:if #:func)
                  