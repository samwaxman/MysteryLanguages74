#lang racket
(require "../ML-Helpers.rkt")
(provide first rest link
         is-empty is-link)

;Gets the first element of a non empty list
(define (first x)
  (if (cons? x) (car x)
      (raise-user-error (~a "first expected a non-empty list but received " (~my-s x)))))

;Gets all but the first element of a non empty list
(define (rest x)
  (if (cons? x) (cdr x)
      (raise-user-error (~a "rest expected a non-empty list but received " (~my-s x)))))

;Links an element onto a list
;Unlike racket, y must be a list. There are no pairs in this language.
(define (link x y)
  (if (list? y)
      (cons x y)
      (raise-user-error (~a "link expected a list for argument 2 but received " (~my-s y)))))

(define is-empty (procedure-rename null? 'is-empty))
(define is-link (procedure-rename cons? 'is-link))