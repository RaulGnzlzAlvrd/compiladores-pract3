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

;;
;; A lot of examples.
;;

(display "Example 1: 3 - (3 / 6)\n")
(let ((input (open-input-string "3 - (3 / 6)")))
  (minHS-parser (lex-this minHS-lexer input)))
#|
Desired response:
(prim-exp #<procedure:-> (num-exp 3) (par-exp (prim-exp #<procedure:/> (num-exp 3) (num-exp 6))))
|#

(display "\nExample 2: if(#t)then{2}else{3}\n")
(let ((input (open-input-string "if(#t)then{2}else{3}")))
  (minHS-parser (lex-this minHS-lexer input)))
#|
Desired response:
(if-exp (bool-exp #t) (num-exp 2) (num-exp 3))
|#

(display "\nExample 3: fun ([x:Int]:Int) => x\n")
(let ((input (open-input-string "fun ([x:Int]:Int) => x")))
  (minHS-parser (lex-this minHS-lexer input)))
#|
Desired response:
(fun-exp (typeof-exp (brack-exp (typeof-exp (var-exp 'x) (int-exp))) (int-exp)) (var-exp 'x))
|#

(display "\nExample 4: fun ([f:Func Int Int]:Int) => f app 1\n")
(let ((input (open-input-string "fun ([f:Func Int Int]:Int) => f app 1")))
  (minHS-parser (lex-this minHS-lexer input)))
#|
Desired response:
(fun-exp (typeof-exp (brack-exp (typeof-exp (var-exp 'f) (func-exp (int-exp) (int-exp)))) (int-exp)) (app-exp (var-exp 'f) (num-exp 1)))
|#

(display "\nExample 5: fun ([f:Func (Func Int Bool) Int]:Bool) => #t\n")
(let ((input (open-input-string "fun ([f:Func (Func Int Bool) Int]:Bool) => #t")))
  (minHS-parser (lex-this minHS-lexer input)))
#|
Desired response:
(fun-exp (typeof-exp (brack-exp (typeof-exp (var-exp 'f) (func-exp (par-exp (func-exp (int-exp) (boole-exp))) (int-exp)))) (boole-exp)) (bool-exp #t))
|#

(display "\nExample 5.1: fun ([a:Bool][b:Bool]:Bool) => a or b and c and d\n")
(let ((input (open-input-string "fun ([a:Bool][b:Bool]:Bool) => a or b and c and d")))
  (minHS-parser (lex-this minHS-lexer input)))
#|
Desired response:
(fun-exp
 (typeof-exp
  (brack-exp
   (app-t-exp
    (typeof-exp (var-exp 'a) (boole-exp))
    (typeof-exp (var-exp 'b) (boole-exp))))
  (boole-exp))
 (prim-exp
  'or
  (var-exp 'a)
  (prim-exp
   'and
   (prim-exp 'and (var-exp 'b) (var-exp 'c))
   (var-exp 'd))))
|#

(display "\nExample 6: funF (sumita ([x:Int][y:Int]):Int) => x+y\n")
(let ((input (open-input-string "funF (sumita ([x:Int][y:Int]):Int) => x+y")))
  (minHS-parser (lex-this minHS-lexer input)))

#|
Desired response:
(fun-f-exp
 (var-exp 'sumita)
 (typeof-exp
  (brack-exp
   (app-t-exp
    (typeof-exp (var-exp 'x) (int-exp))
    (typeof-exp (var-exp 'y) (int-exp))))
  (int-exp))
 (prim-exp #<procedure:+> (var-exp 'x) (var-exp 'y)))
|#

(display "\nExample 7: let ([x:Int = 1][y:Int = 2][3:Int = 3]) in x+y*z end\n")
(let ((input (open-input-string "let ([x:Int = 1][y:Int = 2][3:Int = 3]) in x+y*z end")))
  (minHS-parser (lex-this minHS-lexer input)))
#|
Desired response:
(let-exp
 (brack-exp
  (app-t-exp
   (app-t-exp
    (assign-exp (typeof-exp (var-exp 'x) (int-exp)) (num-exp 1))
    (assign-exp (typeof-exp (var-exp 'y) (int-exp)) (num-exp 2)))
   (assign-exp (typeof-exp (num-exp 3) (int-exp)) (num-exp 3))))
 (prim-exp
  #<procedure:+>
  (var-exp 'x)
  (prim-exp #<procedure:*> (var-exp 'y) (var-exp 'z))))

|#

(display "\nExample 7.1: let ([x:Int = 1 + 1]) in x+y end\n")
(let ((input (open-input-string "let ([x:Int = 1 + 1]) in x+y end")))
  (minHS-parser (lex-this minHS-lexer input)))
#|
Desired response:
(let-exp
 (brack-exp (assign-exp (typeof-exp (var-exp 'x) (int-exp)) (prim-exp #<procedure:+> (num-exp 1) (num-exp 1))))
 (prim-exp #<procedure:+> (var-exp 'x) (var-exp 'y)))
|#

(display "\nExample 8: ((funF (sumita ([x:Int][y:Int]):Int) => x+y) app 2) app 4\n")
(let ((input (open-input-string "((funF (sumita ([x:Int][y:Int]):Int) => x+y) app 2) app 4")))
  (minHS-parser (lex-this minHS-lexer input)))
#|
(app-exp
 (par-exp
  (app-exp
   (par-exp
    (fun-f-exp
     (var-exp 'sumita)
     (typeof-exp
      (brack-exp
       (app-t-exp
        (typeof-exp (var-exp 'x) (int-exp))
        (typeof-exp (var-exp 'y) (int-exp))))
      (int-exp))
     (prim-exp #<procedure:+> (var-exp 'x) (var-exp 'y))))
   (num-exp 2)))
 (num-exp 4))
|#
