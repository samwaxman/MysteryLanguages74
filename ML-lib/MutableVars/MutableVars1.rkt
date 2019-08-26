#lang racket
(require "../AnonymousFunctions/AnonymousFunctions1.rkt")
(require (only-in "../NamedFunctions/NamedFunctions1.rkt" #%func))
(provide (all-from-out "../AnonymousFunctions/AnonymousFunctions1.rkt")
         #%func)
