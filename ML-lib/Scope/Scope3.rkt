#lang racket
;TURN THIS INTO MACROS AS FOLLOWS:
;HOIST RETURNS A SYNTAX LIST WHOSE FIRST ELEMENT IS THE TRANSFORMED SYNTAX
;WHOS SECOND ELEMENT IS A LIST OF THE LIFTED IDS

;HOIST-MODULE RETURNS ONLY THE SYNTAX

;MODULE-begin is (NF1-#%module-begin (hoist-module body ...))



(require
         (for-syntax racket
                     syntax/parse)
         (prefix-in NF1- "../NamedFunctions/NamedFunctions1.rkt")
         (rename-in "../ML-Helpers.rkt" [#%arity-app #%app])
         (only-in "Scope1.rkt" #%reassign))
(provide
 (except-out (unprefix-out NF1- "../NamedFunctions/NamedFunctions1.rkt") NF1-#%module-begin)
 #%reassign
 (rename-out [my-mod #%module-begin]))

(define-for-syntax (remove-syntax-duplicates list)
  (remove-duplicates list (lambda (x y) (eq? (syntax->datum x) (syntax->datum y)))))

;Now that there's no splicing (we only print one value), we likely can get
;away with wrapping this hoist-module into regular hoist
(define-for-syntax (hoist-module stx vars)
  (syntax-case stx ()
    [(body ... last-body)
     ;;These calls to hoisted will update vars so we know
     ;;what needs to be hoisted
     (with-syntax ([(hoisted ...)
                    (map (lambda (x) (hoist x vars))
                         (syntax->list #'(body ... last-body)))])
       ;;Now that we know, we can bind them to undefine
       (with-syntax ([(hoisted-vars ...) (map (lambda (x) #`[#,x 'Undefined]) (remove-syntax-duplicates (unbox vars)))])
         #'(NF1-#%module-begin (let (hoisted-vars ...) hoisted ...))))]))


(define-for-syntax (hoist stx vars)
  (syntax-parse stx #:literals(NF1-#%func NF1-#%let NF1-#%id NF1-#%block)
    [(NF1-#%block body ...)
     (define fresh-vars (box '()))
       
       ;as above, we hoist all the inter expressions first,
       ;;which updates fresh-vars and lets us know what to hoist
       (with-syntax ([(hoisted ...)
                      (map (lambda (x) (hoist x fresh-vars))
                           (syntax->list #'(body ...)))])
         
         (with-syntax ([(hoisted-vars ...) (unbox fresh-vars)])
           #'(NF1-#%block (let* ([hoisted-vars 'Undefined] ...) hoisted ...))))]
    [(NF1-#%func name(args ...) body ... last-body)
     (let ()
       ;;variables hoist to the top of the function, so
       ;;functions get their own new fresh set of vars
       (define fresh-vars (box '()))
       
       ;as above, we hoist all the inter expressions first,
       ;;which updates fresh-vars and lets us know what to hoist
       (with-syntax ([(hoisted ...)
                      (map (lambda (x) (hoist x fresh-vars))
                           (syntax->list #'(body ... last-body)))])
         
         (with-syntax ([(hoisted-vars ...) (unbox fresh-vars)])
           #'(NF1-#%func name(args ...) (let* ([hoisted-vars 'Undefined] ...) hoisted ...)))))]
    
    [(NF1-#%let ([id binding] ...) body ... last-body)
     (let ([id-list (syntax->list #'(id ...))])
       ;set the vars to know we need to hoist this variable
       (set-box! vars (remove-syntax-duplicates (append id-list (unbox vars))))
       ;hoist everything so we see know what vars must be lifted. Order isn't important
       ;unless you want syntax highlighting
       (with-syntax ([(bindings ...) (map (lambda (x) (hoist x vars)) (syntax->list #'(binding ...)))]
                     [(ids ...) (map (lambda (x) (hoist x vars)) (syntax->list #'(id ...)))]
                     [(bodies ...) (map (lambda (x) (hoist x vars)) (syntax->list #'(body ... last-body)))])
         #'(let () (set! ids bindings) ... (let () bodies ...))))]
    [(op args ...) (with-syntax ([(hoisted ...)
                                  (map (lambda (x) (hoist x vars))
                                       (syntax->list #'(op args ...)))])
                     #'(hoisted ...))]
    [atomic #'atomic]))

(define-syntax (my-mod stx)
  (syntax-case stx ()
    [(_ body ...)
     
     ;Note, vars is a boxed list so that we can pass it by reference and that we can modify
     ;it's contents when we pass it into other functions. Probably a mutable list could accomplish this?
     (hoist-module #'(body ...) (box '()))]))








