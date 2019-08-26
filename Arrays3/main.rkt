#lang racket
;This prefix in is so that this file doesn't have the
;notions of module-begin, app, etc defined in numbers
(require (prefix-in #% "../ML-lib/Arrays/Arrays3.rkt")
         "../ML-lib/ML-Helpers.rkt")
(provide (unprefix-out #% "../ML-lib/Arrays/Arrays3.rkt"))


(setup-reader Arrays1
              #:let #:+ #:- #:* #:/
              #:arrays)