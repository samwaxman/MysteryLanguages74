#lang racket
(require (prefix-in F1- "../Fields/Fields1.rkt")
         "EvaluationOrderSetup.rkt"
         "../ML-Helpers.rkt")
(provide
 (except-out (unprefix-out F1- "../Fields/Fields1.rkt") F1-#%reassign))

;Nothing special about this language variant. Just like Fields 1.