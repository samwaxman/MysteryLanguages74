#lang racket
#|Scope README

Scope1.rkt Regular expected lexical scope, just like racket.
Scope2.rkt Every variable is a global variable.
Scope3.rkt Variable hoisting. Any variable gets hoisted up to the nearest
function definition or module and is bound to undefined. Then when you reach the let,
it reassigns it.
Scope4.rkt Reassigning an unbound identifier will bind it.

Syntax highlighting is turned off for 3 and 4. I'll turn it off for 1 and 2 later.
|#
