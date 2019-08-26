#lang racket
;This prefix in is so that this file doesn't have the
;notions of module-begin, app, etc defined in numbers
(require (prefix-in #% "../ML-lib/Arrays/Arrays2.rkt")
         "../ML-lib/ML-Helpers.rkt")
(provide (unprefix-out #% "../ML-lib/Arrays/Arrays2.rkt"))


(setup-reader Arrays2
              #:let #:+ #:- #:* #:/
              #:arrays)