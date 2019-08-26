#lang racket
(require racket/stxparam
         "../ML-Helpers.rkt"
         (prefix-in F1- "../Fields/Fields1.rkt"))

(provide my-app #%let #%func #%lambda
         #%id parameterized-mod-begin wrap)

(define-syntax-parameter lazy unbound)
(define evaluate (make-parameter (lambda (x) (error "Unbound parameter: evaluate"))))


;struct and printer for user defined functions.
;used so we know what to make lazy as opposed to sending
;in lazy argument even to procedures like +
(define (func-print rec port mode)
  (begin
    (write-string "#<procedure" port)
    ;(write-string (function-name rec) port)
    (write-string ">" port)))

(struct function (lam)
  #:methods gen:custom-write
  [(define write-proc func-print)])


;Lazy let makes the binding lazy
(define-syntax-rule (#%let ([id binding]) body ... last-body)
  (let ([id (lazy binding)]) body ... last-body))

;Lazy func makes the overall binding to name lazy
(define-syntax-rule (#%func name (args ...) body ... last-body)
  (define name (lazy (function (procedure-rename
                                (lambda (args ...) (check-decrement-fuel)
                                  body ... last-body)
                                'name)))))

(define-syntax-rule (#%lambda (args ...) body ... last-body)
  (function (procedure-rename (lambda (args ...) (check-decrement-fuel)
                                body ... last-body)
                              (string->symbol "anonymous function"))))

(define-syntax-rule (my-app proc args ...)
  (let ([evaluated-proc proc])
    (if (function? evaluated-proc)
        (#%arity-app (function-lam evaluated-proc)
                     ;Note this means that (#%id x) turns into
                     ;(lazy (#%id x)), and (lazy 1) would get double
                     ;wrapped into (lazy (lazy 1))
                     (lazy args) ...)
        (#%arity-app evaluated-proc args ...))))

(struct wrap (delayed-expr))
(define (strict val)
  (if (wrap? val)
      (strict (wrap-delayed-expr val))
      ((evaluate) val)))

(define-syntax-rule (#%id x)
  (strict x))

(define-syntax-rule (parameterized-mod-begin #:wrap lazy-imp #:unwrap eval-imp body ...)
  (F1-#%module-begin (syntax-parameterize ([lazy (to-syntax-rules lazy-imp)])
                       (parameterize ([evaluate eval-imp])
                         ;In case there's an empty module
                         (values)
                         body ...))))




