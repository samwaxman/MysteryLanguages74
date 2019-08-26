#lang racket
(require racket/splicing
         (for-syntax syntax/parse)
         "../ML-Helpers.rkt"
         syntax/macro-testing)
(provide ML+ ML- ML* ML/  ML-let
         add-setter
         subtract-setter
         multiply-setter
         divide-setter
         my-top
         #%id
         #%block
         parameterized-mod-begin)



;Defines functions for add subtract multiply and divide
;and defines setters for them
(make-shared-variable add)
(make-shared-variable subtract)
(make-shared-variable multiply)
(make-shared-variable divide)

(define (ML+ num1 num2)
  ;check-contract is checking the contract on the arguments
  ;if it fails it throws a custom error message
  (check-contract '+ (num1 num2) (real? real?) ("number" "number"))
  (add num1 num2))

(define (ML- num1 num2)
  (check-contract '- (num1 num2) (real? real?) ("number" "number"))
  (subtract num1 num2))

(define (ML* num1 num2)
  (check-contract '* (num1 num2) (real? real?) ("number" "number"))
  (multiply num1 num2))

(define (ML/ num1 num2)
  (check-contract '/ (num1 num2) (real? real?) ("number" "number"))
  (divide num1 num2))

(define-syntax (ML-let stx)
  (syntax-parse stx
    [(_ ([name binding]) body ... last-body)
     #'(let ([name binding]) body ... last-body)]))

;Racket doesn't have a wrapper for
;when variables turn into values
;like it does for #%datum and such
;so we made one! For now, it's the identity
;transform
(define-syntax-rule (#%id id)
  id)

;Racket normally treats unbound identifiers as syntax errors. This
;bumps the error down to runtime.
(define-syntax-rule (my-top . var)
  (raise-user-error
   (~a "Unbound identifier: " 'var)))

;Adds blocks to the language
(define-syntax-rule (#%block body ...)
  (let () body ...))

(define-syntax-rule (parameterized-mod-begin
                     add subtract multiply
                     divide body ...)
  (#%module-begin
   (add-setter add) ;sets our shared variables to their proper value
   (subtract-setter subtract)
   (multiply-setter multiply)
   (divide-setter divide)
   (fill-tank) ;For modules that have functions, this resets how many times
   ;they can call a function before they time out.
   (display-compile-time-error) ;This sets the display handler for errors
   ;It will make the two pesky racket compile time errors appear as I want them to,
   ;but ONLY for the numbered languages. The languages that give answers for the entire language set
   ;are dealt with seperately.
   (transform-bad-compile-errors
    (convert-compile-time-error ;This bumps down errors from compile time to runtime
     ;I'm using it for duplicate function name error and set!: unbound identifier in module error
     ;then using other methods to convert these errors.
     (let ([%blue-output-port (void)]) ; These lines are a bit of a hack to get output printed as blue instead of purple.
       ;%blue-output-port will be in scope during program, but the % means students can't access it
       (global-port-print-handler (lambda (a b) (set! %blue-output-port b))) ; Current-print always prints to the blue output port, so we use
       ;the global-port-print handler to capture the blue port
       ((current-print) "dummy") ;Send the blue port to the global port print handler
       (setup-printer) ;Makes everything that gets printed be in special format - resets the global-port-print-handler to what we actually want it to be
       (parameterize ([current-output-port %blue-output-port]) ; After capturing the blue port, we parameterize the current output port to be the blue one
     (let () body ...)))))))

