#lang racket
(require "../ML-lib/ML-Helpers.rkt")
(provide #%module-begin #%datum #%top #%app)

(setup-all-reader (Fields1 Fields2 Fields3 Fields4 Fields5)
                  ("------Core 1------" "------Core 2------" "------Advanced 1------"
                                        "------Prank 1------" "------Prank 2------") 
                  #:let #:+ #:- #:* #:/
                  #:> #:< #:>= #:<= #:== #:!= #:++
                  #:if #:func #:reassign #:lambda #:record)