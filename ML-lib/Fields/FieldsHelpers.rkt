#lang racket
(require (prefix-in MV- "../MutableVars/MutableVars1.rkt"))
(provide #%reassign-field #%record #%access
         parameterize-mod
         return-transformer
         field-condition
         field-transformer
         value-transformer
         (struct-out rec)
         student-print)
(require "../ML-Helpers.rkt"
         racket/format
         racket/stxparam
         (for-syntax syntax/parse))


;;The three things that vary depending on the Fields language
;that need to be syntax parameters rather than just regular
;parameters
(define-syntax-parameter field-condition
  unbound)
(define-syntax-parameter field-transformer
  unbound)
(define-syntax-parameter value-transformer
  unbound)
;Note, making return-transformer a parameter failed.
;Evidently the call to the printer happens outside of the parameterize
;scope (bizarre). Do tests with the shared variables and the language
;that prints all the other languages at once, make sure the mutation on the
;return transformer doesn't mess up any of the others

;(If it does, no biggy, just place this language last, but it's good
;to know)
(make-shared-variable return-transformer x)
(make-shared-variable on-create-bad-field x)
(make-shared-variable on-access-bad-field x)



;CREATING THE RECORD STRUCT
(define (record-to-string record depth)
  (define recordString
    (foldl (lambda (key string)
             (~a string (~my-s key) ":"
                 (field-to-string depth
                                  (return-transformer (hash-ref (rec-mapping record)
                                                                key)))
                                                       
                 ", "))
           "{"
           (rec-order record)))
  (if (= (string-length recordString) 1) "{}"
      (string-append (substring recordString 0 (- (string-length recordString) 2)) "}")))

;Handles records that could potentitally be infinitely deep.
;;Elipses them out after a depth of 3
(define (field-to-string depth field)
  (if (and (= depth 3) (rec? field)) (~a "...")
      (if (rec? field) (record-to-string field (+ depth 1))
          (~my-s field))))

(define (rec-print rec port mode)
  ;This prevents racket's weird recursive printing stuff
  ;that causes lazy record fields to be evaluated twice
  (if (equal? (object-name port) 'null) (write-string "" port)
      (write-string (record-to-string rec 0) port)))

(struct rec (mapping order)
  #:methods gen:custom-write
  [(define write-proc rec-print)])

;REASSIGN FIELD
(define-syntax-rule (#%reassign-field rec field value)
  (let ([eval-rec rec])
    (begin
      ;can format this one like the others if you want (field reassignment expected a record ... given x)
      (if (rec? eval-rec) (values)
          (raise-user-error (~a "Field reassignment expected a record to reassign the field of. Received "
                                (~my-s eval-rec))))
      (let-values ([(condition-held? eval-field) (field-condition field)]
                   ;reassignment is strict even in the lazy lang
                   [(eval-val) value])
        (let ([field-T (field-transformer eval-field)] [value-T (value-transformer eval-val)])
          (if condition-held?
              (begin
                (hash-update! (rec-mapping eval-rec) field-T
                              (lambda (_) value-T)
                              (lambda () (raise-user-error (string-append "Field not found: " (~my-s field-T)))))
                (return-transformer value-T))
              (on-access-bad-field field-T)))))))

;Might be better to do error checking before doing the hash-sets and appends tbh.
;;Same functionality, but contracts should probably checked up front instead of recursively
(define-syntax (add-field-binding stx*)
  (syntax-parse stx*
    [(_ field value map order)
     ;check if the condition to be a field name holds, and if not, throw a bad field error
     #'(let-values ([(condition-held? eval-field) (field-condition field)])    
         (let ([field-T (field-transformer eval-field)] [value-T (value-transformer value)])
           (if condition-held?
               (begin
                 (hash-set! map field-T value-T)
                 (if (member field-T order) order (append order (list field-T))))
               (on-create-bad-field field-T))))]))


;The order in these structs is so we know what value to print
;the fields (in the order they're defined)
(define-syntax-rule (#%record ([field value]...))
  (let ([mapping (make-hash)] [order '()])
    (let* ([order (add-field-binding field value mapping order)] ...)
      (rec mapping order))))



(define-syntax-rule (#%access rec field)
  (begin
    (check-decrement-fuel)
    (let ([eval-rec rec])
      (if (rec? eval-rec) (values)
          (raise-user-error (~a "Field access expected a record to access. Received "
                                (~my-s eval-rec))))
      (let-values ([(condition-held? eval-field) (field-condition field)])
        (let ([field-T (field-transformer eval-field)])
          (if condition-held?
              (return-transformer
               (hash-ref! (rec-mapping eval-rec) field-T
                          (lambda () (raise-user-error (string-append "Field not found: " (~my-s field-T))))))
              (on-access-bad-field field-T)))))))


(define-syntax-rule (parameterize-mod field-cond value-trans
                                      return-trans field-trans
                                      on-create-bad-field
                                      on-access-bad-field body ...)
  (MV-#%module-begin
   (syntax-parameterize
       ([field-condition (to-syntax-rules field-cond)]
        [value-transformer (to-syntax-rules value-trans)]
        [field-transformer (to-syntax-rules field-trans)])
     (return-transformer-setter return-trans)
     (on-create-bad-field-setter on-create-bad-field)
     (on-access-bad-field-setter on-access-bad-field)
     body ...)))
