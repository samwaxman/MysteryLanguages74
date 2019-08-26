#lang racket
(require "GuilessBrIndent.rkt")
(require racket)

(define INDENT-SIZE 2)

(define (left-bracket? c) (member c (list #\{ #\[)))
(define (right-bracket? c) (member c (list #\} #\])))
(define (allowed? prevAllowed char)
  (or (char-whitespace? char)
      (and (not (char-alphabetic? char))
           (and prevAllowed (or (char-numeric? char) (member char (list #\_ #\+ #\- #\* #\\)))))))
(define (contains-keyword-at-point? keyword-char-list char-list prevAllowed?)
  (let ([first (car char-list)])
    (and prevAllowed? (list-prefix? keyword-char-list char-list)
         (or (= (length keyword-char-list) (length char-list))
             (let ([next-char (list-ref  char-list (length keyword-char-list))])
               (not (or (char-alphabetic? next-char) (char-numeric? next-char)
                        (member char (list #\_ #\+ #\- #\* #\\)))))))))

(define indent-keyword-ends (list (cons "var" "end") (cons "struct" "end") (cons "for" "end") (cons "class" "end") (cons "if" "else-unindent") (cons "else-indent" "end") (cons "fun" "end") (cons "lam" "end") (cons "block" "end")))
(define indent-keywords (list "for" "if" "else-indent" "fun" "lam" "block" "struct" "class" "var"))
(define unindent-keywords (list "else-unindent" "end"))

(define (determine-active-words word-list)
  (cond
    [(null? word-list) word-list]
    [(and (cons? word-list) (member (car word-list) indent-keywords))
     (if (member (dict-ref indent-keyword-ends (car word-list)) (cdr word-list))
         (determine-active-words (remove (dict-ref indent-keyword-ends (car word-list)) (cdr word-list)))
         (cons (car word-list) (determine-active-words (cdr word-list))))]
    [(cons? word-list) (cons (car word-list) (determine-active-words (cdr word-list)))]))

(define (walk-line line)
  (define words-found '())
  (define (helper remaining-line prevAllowed?)
    (cond
      [(cons? remaining-line)
       (begin
         (for-each (lambda (word)
                     (if (contains-keyword-at-point?
                          (string->list word)
                          remaining-line
                          prevAllowed?)
                         (if (equal? word "else")
                             (set! words-found (append words-found (list "else-unindent" "else-indent")))
                             (set! words-found (append words-found (list word))))
                         (values)))
                   ;Not great, but it's a weird case
                   (cons "else" (remove "end-indent" (remove "end-unindent" (append indent-keywords unindent-keywords)))))
         (helper (cdr remaining-line) (allowed? prevAllowed? (car remaining-line))))]
      [(null? remaining-line) words-found]))
  (helper line #t))

(define (determine-indent-from-previous previous-line)
  (define indent-add 0)
  (let* ([walked-line (walk-line previous-line)]
         [active-words (determine-active-words (walk-line previous-line))])
    (for-each (lambda (word) (if (member word indent-keywords)
                                 (set! indent-add (+ indent-add INDENT-SIZE))
                                 (values)))
              active-words)
    indent-add))

(define (determine-unindent-from-current current-line)
  (define unindent-add 0)
  (let ([active-words (determine-active-words (walk-line current-line))])
    (for-each (lambda (word) (if (member word unindent-keywords)
                                 (set! unindent-add (- unindent-add INDENT-SIZE))
                                 (values)))
              active-words)
    unindent-add))

(define (indent-ML tbox [posn 0])
  (define prev-line (previous-line tbox posn))
  (define current-line (line tbox posn))
  (define prev-indent (or (line-indent tbox prev-line) 0))
  (define prev-line-chars (or (and prev-line (line-chars tbox prev-line)) '()))
  (define cur-line-chars (line-chars tbox current-line))
  (define indent-forward (determine-indent-from-previous prev-line-chars))
  (define indent-back (determine-unindent-from-current cur-line-chars))
  (define indent
    (+ prev-indent indent-forward indent-back))
  (if (> indent 0)
      indent
      0))

(provide
 indent-ML)