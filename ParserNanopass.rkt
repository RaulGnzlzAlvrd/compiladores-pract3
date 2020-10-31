#lang nanopass

#|
Compiladores 2021-1
Profesora: Dra. Lourdes del Carmen Gonzalez Huesca
Ayudante: Juan Alfonso Garduño Solís
Laboratorio: Fernando Abigail Galicia Mendoza

Our first approach with nanopass.
|#


#|
¿Podrías explicar la sintaxis de las metavariables para define-language e0, e1, e*, y puntos ...?
x|

¿Cómo se elimina una producción al extender un lenguaje?
O en otras palabras ¿que iría en los ... de (-production-clause ...)?

¿Que son los formals en define-pass?

¿Podrías explicar la sintáxis que se usa en define-pass?
¿Por qué  las comas ,? Ej: ( funF ,x ([ , x * ,t *] ...) antes de la x y t

¿Las transformaciones definidas en los passes las ocupa el parser o en qué momento las vamos a utilizar? 

|#


(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre)
         (prefix-in re- parser-tools/lex-sre)
         parser-tools/yacc)

; Calling the script of "Practica 2 - Parte 2"
(require "parser.rkt")

; Function that returns the string representation of a ASA
(define (expr->string e)
  (match e
    [(var-exp e) (symbol->string e)]
    [(num-exp e) (number->string e)]
    [(bool-exp e) (format "~a" e)]

    [(prim-exp + e1 e2) (string-append "(+ " (expr->string e1) " " (expr->string e2) ")")]

    ))

; The definition of our language
(define-language LF
  (terminals
   (variable (x))
   (primitive (pr))
   (constant (c))
   (type (t)))
  (Expr (e body)
    x
    pr
    c
    t
    (pr e* ... e)
    (begin e* ... e)
    (if e0 e1)
    (if e0 e1 e2)
    (fun ((x* t*) ...) t body* ... body)
    (let ((x* t* e*) ...) body* ... body)
    (funF x ((x* t*) ...) t body* ... body)
    (e0 e1 ...)
    ))

;Some predicates
(define (variable? x) (symbol? x))

(define (type? t)
 (or (equal? t 'Int) (equal? t 'Bool)))

(define (constant? x)
  (or (number? x) (boolean? x)))

(define (pr? x) (#t))

; The parser of LF
(define-parser parse-LF LF)



; A function that make explicit the ocurrences of the begin
(define-pass make-explicit : LF (ir) -> LF ()
  (Expr : Expr (ir) -> Expr ()
    [,c `',c]
    [(fun ([,x* ,t*] ...) ,t ,[body*] ... ,[body])
     `(fun ([,x* ,t*] ...) t (begin ,body* ... ,body))]
    [(let ([,x* ,t* ,[e*]] ...) ,[body*] ... ,[body])
     `(let ([,x* ,t* ,e*] ...) (begin ,body* ... ,body))]
    [(funF ,x ([,x* ,t*] ...) ,t ,[body*] ... ,[body])
     `(funF x ([,x* ,t*] ...) t (begin ,body* ... ,body))]))

(define-language LNI
  (extends LF)
  (Expr (e body)
        (- (if e0 e1))))

(define-parser parse-LNI LNI)

(define-pass rm-armed-if : LF (ir) -> LNI ()
  (Expr : Expr (ir) -> Expr ()
    [,c `',c]
    [(if ,[e0] ,[e1])
     `(if ,e0 ,e1 (void))]))



; Concrete expression;
; (33 + 2)
;(expr->string (par-exp (prim-exp + (num-exp 33) (num-exp 2))))
; Answer: "(+ 33 2)"

; Concrete expression
; 3 - (3 / 6)
;(expr->string (prim-exp - (num-exp 3) 
;    (par-exp (prim-exp - (num-exp 3) (num-exp 6)))))
; Answer "(- 3 (/ 3 6))"

; Concrete expression:
; if(#t and #f)then{2}else{3}
;(expr->string (if-exp (prim-exp 'and (bool-exp #t) (bool-exp #f))
;    (num-exp 2) (num-exp 3)))
; Answer: "(if (and #t #f) 2 3)"

; Concrete expression:
; fun ([x:Int]:Int) => x
;(expr->string (fun-exp (typeof-exp (brack-exp (typeof-exp (var-exp 'x)
;    (int-exp))) (int-exp)) (var-exp 'x)))
; Answer: "(fun ((x Int)) Int x)"

; Concrete expression:
; fun ([x:Int][y:Int]:Int) => x*y
;(expr->string (fun-exp
; (typeof-exp (brack-exp (app-t-exp 
;    (typeof-exp (var-exp 'x) 
;        (int-exp)) (typeof-exp (var-exp 'y) (int-exp)))) (int-exp))
; (prim-exp * (var-exp 'x) (var-exp 'y))))
; Answer: (fun ((x Int) (y Int)) Int (* x y))"

; Concrete expression:
; funF (sumita ([x:Int][y:Int]):Int) => x+y
;(fun-f-exp
; (typeof-f-exp (var-exp 'sumita) (brack-exp 
;    (app-t-exp (typeof-exp (var-exp 'x) (int-exp)) 
;    (typeof-exp (var-exp 'y) (int-exp)))) (int-exp))
; (prim-exp + (var-exp 'x) (var-exp 'y)))
; Answer: "(funF sumita ((x Int) (y Int)) Int (+ x y))"