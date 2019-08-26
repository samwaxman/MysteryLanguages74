#lang racket

(require
         (for-syntax syntax/parse)
         (for-syntax racket)
         (prefix-in NF1- "../NamedFunctions/NamedFunctions1.rkt")
         (only-in "Scope1.rkt" #%reassign)
         (rename-in "../ML-Helpers.rkt" [#%arity-app #%app]))
(provide
 (except-out (unprefix-out NF1- "../NamedFunctions/NamedFunctions1.rkt") NF1-#%module-begin)
 #%reassign
 (rename-out [my-mod #%module-begin]))

;This language turns reassignment into binding statements
;if the variable is unbound at the time that the reassignment
;occurs.


;;NOTE THAT THERE IS A VERY TINY "BUG" HERE!
;;for programs where func is used above its definition
;;ie  (func a() (reassign b 3) b) (func b() 1)
;;while (reassign) should be reassigning b, in this case
;;it'll transform into the let, because it doesn' know that b exists yet

;;It would take a good amount of code and a non-recursive sweep to fix that
;;I'd need to first go through every body, see if they're funcs,
;;add them to the variable lists JUST for other funcs, and as funcs still need
;;access to the other bodies, I'd need to store the information somewhere

;;pain in the butt. In reality, students won't notice, as who's going to test the consistency
;;of reassign/let transformation inside a function, reassigning another function that appears below?
;;but still should fix it eventually

;Takes a block and transforms the bodies in the block
;to have reassigns transformed to lets if need be
(define-for-syntax (transform-reassigns stx env)
  (syntax-case stx ()
    [() #'()]
    [(first rest ...)
     ;transform the first body. If it was a reassign, see if it should be a let
     ;otherwise, it's good and we just transform the rest
     (let ([transformed-first (binding-transform #'first env)])
       (with-syntax ([syn-t-first transformed-first])
         (syntax-case transformed-first (#%reassign)                                        
           [(#%reassign id new-value)
            (if (set-member? (unbox env) (syntax->datum #'id))
                (with-syntax ([(remaining ...)
                               (transform-reassigns #'(rest ...) env)])
                  #'(syn-t-first remaining ...))
                (with-syntax ([(remaining ...)
                               (transform-reassigns #'(rest ...)
                                                    (box (set-add (unbox env)
                                                                  (syntax->datum #'id))))])
                  #'((let ([eval-value new-value])
                      (let ([id eval-value]) eval-value remaining ...)))))]
           [_ (with-syntax ([(remaining ...) (transform-reassigns #'(rest ...) env)])
                #'(syn-t-first remaining ...))])))]))

(define-for-syntax (binding-transform stx env)
  (syntax-parse stx #:literals(NF1-#%let NF1-#%func NF1-#%if NF1-#%block)
    ;if it's a let, add the identifier to our env
    [(NF1-#%let ([id binding] ...) body ... last-body)
     (with-syntax ([(bodies ...)
                    ;necessary?
                    (syntax->list
                     (transform-reassigns
                      #'(body ... last-body)
                      (let ([id-list (map (lambda (x) (syntax->datum x)) (syntax->list #'(id ...)))]) 
                        (box (set-union (unbox env) (list->set id-list))))))])
       #'(NF1-#%let ([id binding] ...) bodies ...))]
    ;if its a func, add the func name to the env
    [(NF1-#%func name(args ...) body ... last-body)
     (begin
       (set-box! env (set-add (unbox env) (syntax->datum #'name)))
       (define arg-list (map (lambda (x) (syntax->datum x)) (syntax->list #'(args ...))))
       (with-syntax ([(bodies ...)
                      (syntax->list
                       (transform-reassigns #'(body ... last-body) (box (set-union (unbox env) (list->set arg-list)))))])
         #'(NF1-#%func name(args ...) bodies ...)))]
    [(NF1-#%block expr ...) (with-syntax ([(transformed-exprs ...)
                               (syntax->list
                                (transform-reassigns #'(NF1-#%block expr ...) env))])
                  #'(transformed-exprs ...))]
    [(expr ...) (with-syntax ([(transformed-exprs ...)
                               (map (lambda (x) (car (syntax->list (transform-reassigns #`(#,x) env)))) (syntax->list #'(expr ...)))])
                  #'(transformed-exprs ...))]
    [atomic #'atomic]))

#;(begin-for-syntax
  (print (binding-transform #'(begin (let ([x 1]) 2) 2) (box (set)))))

(define-syntax (my-mod stx)
  (syntax-parse stx
    ;env is boxed set for similar reason to scope 3. we're passing it by reference
    ;so we can modify it in inner functions
    ;(while still using it functionaly in the places we should)
    [(_ body ...)  #`(NF1-#%module-begin
                     #,(binding-transform #'(NF1-#%block body ...) (box (set))))]))