#lang racket
(require racket/class
         racket/list
         racket/string
         racket/contract)
(provide (all-defined-out))


(define indent-width 2)

(define (char text pos)
 ; ((is-a?/c text%) (or/c exact-nonnegative-integer? #f) . -> . (or/c char? #f))
  (and pos (send text get-character pos)))

(define(line text pos)
  ;((is-a?/c text%) (or/c exact-nonnegative-integer? #f) . -> . exact-nonnegative-integer?)
  (send text position-line pos))

(define (line-chars text line)
  ;((is-a?/c text%) exact-nonnegative-integer? . -> . (or/c (listof char?) #f))
  (and
   (valid-line? text line)
   (for/list ([pos (in-range (line-start text line) (add1 (line-end text line)))])
             (char text pos))))



(define (previous-line text pos)
  ;((is-a?/c text%) exact-nonnegative-integer? . -> . (or/c exact-nonnegative-integer? #f))
  (define this-line (line text pos))
  (and (this-line . > . 0) (sub1 this-line)))


(define (next-line text pos)
  ;((is-a?/c text%) exact-nonnegative-integer? . -> . (or/c exact-nonnegative-integer? #f))
  (define last (send text last-line))
  (define this-line (line text pos))
  (and (this-line . < . last) (add1 this-line)))


(define (valid-line? text line)
  (and line (<= 0 line (send text last-line))))

(define (line-start text line)
  (and (valid-line? text line)
       (send text line-start-position line)))

(define (line-end text line)
  (and (valid-line? text line)
       (send text line-end-position line)))


(define (first-visible-char-pos text start end)
  ;; private
  (for*/first ([pos (in-range start end (if (start . > . end) -1 1))]
               [c (in-value (char text pos))]
               #:when (not (char-blank? c)))
              pos))

(define (line-start-visible text line)
  ;((is-a?/c text%) (or/c exact-nonnegative-integer? #f) . -> . (or/c exact-nonnegative-integer? #f))
  (define start (line-start text line))
  (define end (line-end text line))
  (and start end (first-visible-char-pos text start end)))

(define (line-first-visible-char text line)
  ;((is-a?/c text%) (or/c exact-nonnegative-integer? #f) . -> . (or/c char? #f))
  (char text (line-start-visible text line)))

(define (line-last-visible-char text line)
  ;((is-a?/c text%) (or/c exact-nonnegative-integer? #f) . -> . (or/c char? #f))
  (char text (line-end-visible text line)))  

(define (line-end-visible text line)
  ;((is-a?/c text%) (or/c exact-nonnegative-integer? #f) . -> . (or/c exact-nonnegative-integer? #f))
  (define start+1 (line-end text line)) ; start before newline
  (define end+1 (line-start text line))
  (and start+1 end+1 (first-visible-char-pos text (sub1 start+1) (sub1 end+1))))


(define (line-indent text line)
  ;((is-a?/c text%) (or/c exact-nonnegative-integer? #f) . -> . (or/c exact-nonnegative-integer? #f))
  (and (valid-line? text line)
       (let ([lsv (line-start-visible text line)])
         (and lsv ; could be #f
              (- (line-start-visible text line) (line-start text line))))))

(define (count-char text c [start 0] [end (send text last-position)])
  (for/sum ([pos (in-range start (add1 end))]
            #:when ((char text pos) . char=? . c))
           1))

#;(define (str->text str)
  (define t (new text%))
  (send t insert-port (open-input-string str))
  t)

(define (space-char? x) (char=? x #\space))

#;(define (apply-indenter indenter t-or-str)
  ;(procedure? (or/c (is-a?/c text%) string?) . -> . string?)
  (define t (if (string? t-or-str) (str->text t-or-str) t-or-str))
  (define indented-t
    (for/fold ([t-acc t])
              ([line-idx (in-range (add1 (send t last-line)))])
      ;; simulate DrR indentation
      ;; by dropping leading spaces and applying new indent.
      (define line-start-pos (line-start t-acc line-idx))
      (define new-indent (indenter t-acc line-start-pos))
      (define new-line-str
        (list->string (append (make-list (or new-indent 0) #\space)
                              (dropf (line-chars t-acc line-idx) space-char?))))
      (send t-acc delete line-start-pos (add1 (line-end t-acc line-idx))) ; add1 to grab ending newline too
      (send t-acc insert new-line-str line-start-pos)
      t-acc))
  (send indented-t get-text))

(define (string-indents str)
  ;(string? . -> . (listof (or/c exact-positive-integer? #f)))
  (for/list ([line (in-list (string-split str "\n"))])
            (define len (length (takef (string->list line) space-char?)))
            (and (exact-positive-integer? len) len)))