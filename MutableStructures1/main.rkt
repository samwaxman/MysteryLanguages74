#lang racket
;This prefix is so this module doesn't use the #%module-begin #%top etc
;defined in those files.
(require (prefix-in ML- "../ML-lib/MutableStructures/MutableStructures1.rkt")
         "../ML-lib/ML-Helpers.rkt")
(provide (unprefix-out ML- "../ML-lib/MutableStructures/MutableStructures1.rkt"))


(setup-reader MutableStructures1
              #:let #:+ #:- #:* #:/
              #:> #:< #:>= #:<= #:== #:!= #:++
              #:if #:record #:lambda #:func #:reassign)