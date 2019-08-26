#lang racket
(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre))
(provide color-ML)



(define-lex-abbrevs
  (left-paren "(")
  (right-paren ")")
  (left-bracket "[")
  (right-bracket "]")
  (left-brace "{")
  (right-brace "}")
  (colon-block "block:")
  (paren-lam "lam(")
  (id-chars (:or #\- #\/ #\_ #\+ #\*))
  (identifier (:or
               (:: (:+ id-chars)
                   (:* (:or id-chars
                            numeric))
                   (:+ alphabetic)
                   (:* (:or alphabetic
                            id-chars
                            numeric)))
                   
               (:: alphabetic
                  (:* (:or alphabetic
                           numeric
                           id-chars)))))
                  
  (opener (:or "if" "lam" "var" "fun" "for" "block:" "block" "struct" "class"))
  (closer "end")
  (just-blue (:or "else" "extends"))
  (string-text (:or "\\\""
                    whitespace
                    (:-
                     (:- any-char
                        #\")
                     #\\)))
  (string-start (:: #\"
              (:* string-text)))
  (string (:: string-start #\"))
  (string-error (:: string-start (:* (:or "\\\"" (:- any-char #\")))))
  (line-comment (:: (:or #\; #\#)
                    (:* (:- any-char
                            #\newline))))
  (anything-else (:- any-char
                             #\")))


(define lex
  (lexer
   [left-paren (values lexeme 'parenthesis '|(| (position-offset start-pos) (position-offset end-pos))]
   [right-paren (values lexeme 'parenthesis '|)| (position-offset start-pos) (position-offset end-pos))]
   ;color wise, parens will match brackets. Unfortunate, but there's only 3 default settings,
   ;and we don't want parens matching fun or end or such
   [left-bracket (values lexeme 'parenthesis '|[| (position-offset start-pos) (position-offset end-pos))]
   [right-bracket (values lexeme 'parenthesis '|]| (position-offset start-pos) (position-offset end-pos))]
   [string (values lexeme 'string #f (position-offset start-pos) (position-offset end-pos))]
   [string-error (values lexeme 'string #f (position-offset start-pos) (position-offset end-pos))]
   ;[left-brace (values lexeme 'parenthesis '|{| (position-offset start-pos) (position-offset end-pos))]
   ;[right-brace (values lexeme 'parenthesis '|}| (position-offset start-pos) (position-offset end-pos))]
   [(eof) (values lexeme 'eof #f #f #f)]
   [opener (values lexeme 'symbol '|{| (position-offset start-pos) (position-offset end-pos))]
   [just-blue (values lexeme 'symbol #f (position-offset start-pos) (position-offset end-pos))]
   [closer (values lexeme 'symbol '|}| (position-offset start-pos) (position-offset end-pos))]
   [identifier (values lexeme 'no-color #f (position-offset start-pos) (position-offset end-pos))]
   ;If you want to do string error, you'll need to mess with the mode argument
   ;Otherwise longest match will mess you up
;   [string-error (values lexeme 'error #f (position-offset start-pos) (position-offset end-pos))]
   [line-comment (values lexeme 'comment #f (position-offset start-pos) (position-offset end-pos))]
   [whitespace (values lexeme 'whitespace #f (position-offset start-pos) (position-offset end-pos))]
   [any-char (values lexeme 'string #f (position-offset start-pos) (position-offset end-pos))]))

(define (color-ML port offset mode)
  (define-values (str color paren-shape start end) (lex port))
      (values str color paren-shape start end 0 0))
;Note, this is suboptimal. It recolors the entire program every keystroke
;but it's the only nice way I found for it to always be right,
;especially with strings.
