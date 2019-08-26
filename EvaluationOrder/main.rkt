#lang racket
(require "../ML-lib/ML-Helpers.rkt")
(provide #%module-begin #%datum #%top #%app)


(setup-all-reader (EvaluationOrder1 EvaluationOrder2 EvaluationOrder3 EvaluationOrder4)
                  ("------Core 1------" "------Core 2------" "------Core 3------" "------Advanced 1------")
                  #:let #:+ #:- #:* #:/
                  #:> #:< #:>= #:<= #:== #:!= #:++
                  #:if #:func #:lambda)
