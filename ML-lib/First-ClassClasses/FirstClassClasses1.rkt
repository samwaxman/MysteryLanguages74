#lang racket
(require "../ML-Helpers.rkt")
(require "Helper.rkt")
(require (prefix-in OB- "../Objects/Objects1.rkt"))
(require test-engine/racket-tests)
(provide
 #%type-check
 #%testI-typed
 #%testE-typed
 (except-out
  (unprefix-out OB- "../Objects/Objects1.rkt")
  OB-#%type-check OB-#%testI-typed OB-#%testE-typed
  ))

(define (full-type-check ast)
  (new-full-type-checker ast default-same-type? default-same-type?no-convert))

(define-syntax-rule (#%type-check code ...)
  (begin
    ;(printf "~s~n" '(code ...)) ;; this is just a line I uncomment when I want to print out the parsed result
    (full-type-check '(code ...))
    code ...))

(define-syntax-rule (#%testI-typed exp args)
  (OB-#%testI exp (#%type-check args)))

(define-syntax-rule (#%testE-typed exp args)
  (OB-#%testE exp (#%type-check args)))