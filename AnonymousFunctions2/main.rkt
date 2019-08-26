#lang racket
;This prefix in is so that this file doesn't have the
;notions of module-begin, app, etc defined in numbers
(require (prefix-in #% "../ML-lib/AnonymousFunctions/AnonymousFunctions2.rkt")
         "../ML-lib/ML-Helpers.rkt")
(provide (unprefix-out #% "../ML-lib/AnonymousFunctions/AnonymousFunctions2.rkt"))


(setup-reader AnonymousFunctions2
              #:let #:+ #:- #:* #:/
              #:> #:< #:>= #:<= #:== #:!= #:++
              #:if #:lambda #:reassign)