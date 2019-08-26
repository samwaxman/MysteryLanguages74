#lang racket
(require "../ML-lib/ML-Helpers.rkt")
(provide #%module-begin #%datum #%top #%app)

(setup-all-reader (Objects1 Objects2 Objects3 Objects4)
                  ("------Core 1------" "------Prank 1------" "------Core 2------" "------Core 3------")
                  #:let #:typed-let #:+ #:- #:* #:/
                  #:> #:< #:>= #:<= #:== #:!= #:++
                  #:if #:typed-func #:struct #:object #:reassign)