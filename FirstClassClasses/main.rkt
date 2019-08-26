#lang racket
(require "../ML-lib/ML-Helpers.rkt")
(provide #%module-begin #%datum #%top #%app)

(setup-all-reader (FirstClassClasses1 FirstClassClasses2 FirstClassClasses3 FirstClassClasses4)
                  ("------Core 1------" "------Core 2------" "------Core 3------" "------Core 4------")
                  #:let #:typed-let #:+ #:- #:* #:/
                  #:> #:< #:>= #:<= #:== #:!= #:++
                  #:if #:typed-func #:struct #:first-class-classes
                  #:object #:reassign)