#lang racket
(require (prefix-in ML- "../Numbers/Numbers1.rkt")
         "../ML-Helpers.rkt")
(provide (unprefix-out ML- "../Numbers/Numbers1.rkt")
         #%array-access
         #%to-array)

(define #%to-array (lambda list-args (list->vector list-args)))


;Returns the array cell of the given index mod the array length.
(define (#%array-access array index)
  (begin
    (when (not (vector? array))
      (raise-user-error
       (~a "Array access requires an array. Given " (~my-s array)"." )))
    (when (not (exact-integer? index))
      (raise-user-error
       (~a "Array indices can only be integers. Given " (~my-s index) ".")))
    (when (= (vector-length array) 0)
      (raise-user-error "Array index out of bounds."))
    (vector-ref array (modulo index (vector-length array)))))
