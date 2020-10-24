#lang nanopass

#|
Compiladores 2021-1
Profesora: Dra. Lourdes del Carmen Gonzalez Huesca
Ayudante: Juan Alfonso Garduño Solís
Laboratorio: Fernando Abigail Galicia Mendoza

Lexer y parser
|#

(require "lexer.rkt"
         (prefix-in : parser-tools/lex-sre)
         (prefix-in re- parser-tools/lex-sre)
         parser-tools/yacc
         parser-tools/lex)
(provide (all-defined-out))

;;
;; An abstraction of the grammar minHS.
;;

;; Constants
(define-struct num-exp (n) #:transparent)  ; For the numbers.
(define-struct bool-exp (b) #:transparent) ; For the booleans.

;; Variables
(define-struct var-exp (i) #:transparent) ; For the variables.

;; Operadores prim
(define-struct prim-exp (op e1 e2) #:transparent) ; For the arithmetic operations.

;; Parentesis y brackets
(define-struct par-exp (exp) #:transparent)     ; For the parenthesis.
(define-struct key-exp (exp) #:transparent)     ; For the keys.
(define-struct brack-exp (exp) #:transparent)   ; For the brackets.
(define-struct app-t-exp (e1 e2) #:transparent) ; For multiple arguments ][
;; Ej: fun ([x:Int][y:Int]:Int) => x+y

;; if-then-else
(define-struct if-then-exp (g e1 e2) #:transparent) ; For the if conditionals.

;; Tipos
(define-struct int-exp () #:transparent)       ; For the Int type.
(define-struct boole-exp () #:transparent)     ; For the Bool type.
(define-struct func-exp (t1 t2) #:transparent) ; Fun Func type
(define-struct typeof-exp (v e) #:transparent) ; For the type of operator ":"
; Note: There is a difference: bool is for values and boole is for type

;; Funciones
(define-struct fun-exp (sign body) #:transparent)        ;; For functions: fun (sign) => body
(define-struct fun-f-exp (name sign body) #:transparent) ;; For functions: fun (name sign) => body

;; Let
(define-struct let-exp (l body) #:transparent)       ;; For let expresion: let (l) in body end
(define-struct assign-exp (var value) #:transparent) ;; For assign expression in let: var = value

;; function application
(define-struct app-exp (e1 e2) #:transparent) ;; For function application: e1 app e2

;;
;; START parser
;;
(define minHS-parser
  (parser
   (start exp)  ; start clause. The exp is the initial symbol where the parser begins the analysis. 
   (end EOF)    ; end clause. The parser ends when it reads the given symbol. In our case, EOF.
   (error void) ; error clause. Here can be some errors presented in the anlysis.
   (tokens a b) ; tokens clause. Here goes our tokens. In our case, we defined the tokens in the lexer script.
   (precs
    (left APPT) ; precs clause. Here we can give some precedence of our language operators.
    (left ASSIGN)
    (left TYPEOF)
    (left OR)
    (left AND)
    (left - +) 
    (left * /))
   (grammar ; grammar clause. Here goes the grammar of minHS.
    (exp
     ;; Constants
     [(NUM) (num-exp $1)] ;; ((Token) (constructor $1 $2 ... $n)) [1,2,3,...,n]
     [(BOOL) (bool-exp $1)]

     ;; Variables
     [(VAR) (var-exp $1)]

     ;; Prim operators
     [(exp + exp) (make-prim-exp + $1 $3)] ; ((e1 e2 e3 .... en) (constructor $1 $2 $3 ... $n))
     [(exp - exp) (make-prim-exp - $1 $3)]
     [(exp * exp) (make-prim-exp * $1 $3)]
     [(exp / exp) (make-prim-exp / $1 $3)]
     [(exp / exp) (make-prim-exp / $1 $3)]
     [(exp AND exp) (make-prim-exp 'and $1 $3)]
     [(exp OR exp) (make-prim-exp 'or $1 $3)]

     ;; If then else expression
     [(IF LP exp RP THEN LK exp RK ELSE LK exp RK) (if-then-exp $3 $7 $11)]

     ;; Types
     [(INT) (int-exp)]
     [(BOOLE) (boole-exp)]
     [(FUNC exp exp) (make-func-exp $2 $3)]
     [(exp TYPEOF exp) (make-typeof-exp $1 $3)]
     [(LP exp RP TYPEOF exp) (make-typeof-exp $2 $5)]

     ;; Parentesis, brackets, keys, multiple parameter ][
     [(LP exp RP) (make-par-exp $2)]
     [(LB exp RB) (make-brack-exp $2)]
     [(exp ASSIGN exp) (make-assign-exp $1 $3)]
     [(exp APPT exp) (make-app-t-exp $1 $3)]

     ;; Functions
     [(FUN LP exp RP ARROW exp) (make-fun-exp $3 $6)]
     [(FUNF LP exp exp RP ARROW exp) (make-fun-f-exp $3 $4 $7)]

     ;; Let
     [(LET LP exp RP IN exp END) (make-let-exp $3 $6)]

     ;; Function application
     [(exp APP exp) (make-app-exp $1 $3)]))))

; A function that stores our lexer into a lambda function without arguments.
(define (lex-this lexer input) (lambda () (lexer input)))
