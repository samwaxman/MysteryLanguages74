#lang racket
;This prefix in is so that this file doesn't have the
;notions of module-begin, app, etc defined in numbers
(require (prefix-in #% "../ML-lib/First-ClassClasses/FirstClassClasses4.rkt")
         "../ML-lib/ML-Helpers.rkt")
(provide (unprefix-out #% "../ML-lib/First-ClassClasses/FirstClassClasses4.rkt"))


(setup-reader FirstClassClasses4
              #:let #:typed-let #:+ #:- #:* #:/
              #:> #:< #:>= #:<= #:== #:!= #:++
              #:if #:typed-func #:struct #:first-class-classes
              #:object #:reassign)