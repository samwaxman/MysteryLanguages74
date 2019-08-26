#lang racket
(require "../ML-lib/ML-Helpers.rkt")
(provide #%module-begin #%datum #%top #%app)

(setup-all-reader (Arrays1 Arrays2 Arrays3)
                  ("------Core 1------" "------Core 2------" "------Core 3------")
                  #:let #:+ #:- #:* #:/
                  #:arrays)