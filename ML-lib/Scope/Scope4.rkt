#lang racket
(require "../ML-Helpers.rkt"
         (prefix-in Cond- "../Conditionals/Conditionals3.rkt"))

(provide
 (except-out
  (unprefix-out Cond- "../Conditionals/Conditionals3.rkt")
  Cond-#%let
  Cond-#%id)
 (rename-out [my-let #%let])
 #%id
 #%func
 #%reassign)

;Global bindings hash table. Stores
;The bindings for every variable bound
(define global-bindings (make-hash))

(define-syntax-rule (my-let ([id binding]) body ... last-body)
  (let ()
  (add-binding 'id binding)
  body ... last-body))


;Adds the binding for the function to the global vars table
;On use, adds the function arguments to the global vars table
(define-syntax (#%func stx)
  (syntax-case stx ()
    [(_ name (args ...) body ... last-body)
       #'(add-binding 'name (procedure-rename
                            (lambda (args ...)
                              (check-decrement-fuel)
                              (add-binding 'args args) ... body ... last-body)
                            'name))]))

;Adds a binding to the global vars table
(define (add-binding id binding)
  (hash-set! global-bindings id binding))

(add-binding 'recap Cond-recap)
(add-binding 'print student-print)

;Reassigns, which is the same thing as shadowing
;in this language
(define-syntax-rule (#%reassign id binding)
  (let ([eval-binding binding])
    (hash-update! global-bindings
                  'id
                  (lambda (x) eval-binding)
                  (lambda () (raise-user-error
                              (~a "Unbound identifier: " 'id))))
    eval-binding))

;;Redirects unbound variables to search for their definition in the global
;;vars table.
(define-syntax-rule (#%id id)
  (hash-ref global-bindings 'id
            (lambda () (raise-user-error (~a "Unbound identifier: " 'id)))))

