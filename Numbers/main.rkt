#lang racket
(require "../ML-lib/ML-Helpers.rkt")
(provide #%module-begin #%datum #%top #%app)

(setup-all-reader (Numbers1 Numbers2 Numbers3)
                  ("------Core 1------" "------Core 2------" "------Core 3------")
                  #:let #:+ #:- #:* #:/)