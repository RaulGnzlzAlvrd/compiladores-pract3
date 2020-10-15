#lang nanopass

#|
Compiladores 2021-1
Profesora: Dra. Lourdes del Carmen Gonzalez Huesca
Ayudante: Juan Alfonso Garduño Solís
Laboratorio: Fernando Abigail Galicia Mendoza

Lexer y parser
|#

(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre)
         (prefix-in re- parser-tools/lex-sre)
         parser-tools/yacc)

;; Tokens que son contenedores --- data Tokens = NUM Int | VAR String | BOOL Bool
(define-tokens a (NUM BOOL  ;; Constants
                      VAR)) ;; Variable
                      
;; Tokens que no almacenan datos
(define-empty-tokens b (+ - * / AND OR          ;; prim
                          LP RP LSB RSB LB RB   ;; parentesis (), brackets {} y square brackets []
                          IF THEN ELSE          ;; condicional
                          BOOLE INT FUNC        ;; Types 
                          FUN FUNF TYPEOF ARROW ;; Funciones
                          LET ASSIGN IN END     ;; Let
                          APP                   ;; Aplicacion de funcion
                          EOF))                 ;; End of line


; Esta definicion pertenece a parsers-tools/lex-ptv-v200
; pero al importar esta biblioteca, causa problemas de sintaxis con los
; operadores aritmeticos
(define-lex-trans (epsilon stx)
  (syntax-case stx ()
    [(_) #'""]))

; Nuestro hermosisimo lexer
(define minHS-lexer
  (lexer
   ;[RegularExpression
   ;=>
   ;Token]

   
   ;; EMPIEZA relacionado a let
   [(:: #\l #\e #\t)
    ; =>
    (cons (token-LET)
          (minHS-lexer input-port))]
   [#\=
    ; =>
    (cons (token-ASSIGN)
          (minHS-lexer input-port))]
   [(:: #\i #\n)
    ; =>
    (cons (token-IN)
          (minHS-lexer input-port))]
   [(:: #\e #\n #\d)
    ; =>
    (cons (token-END)
          (minHS-lexer input-port))]
   ;; TERMINA relacionado a let
   

   ;; EMPIEZA relacionado al if
   [(:: #\i #\f)
    ; =>
    (cons (token-IF)
          (minHS-lexer input-port))]
   [(:: #\t #\h #\e #\n)
    ; =>
    (cons (token-THEN)
          (minHS-lexer input-port))]
   [(:: #\e #\l #\s #\e)
    ; =>
    (cons (token-ELSE)
          (minHS-lexer input-port))]
   ;; TERMINA relacionado al if

   
   ;; EMPIEZA relacionado a tipos
   [#\:
    ; =>
    (cons (token-TYPEOF)
          (minHS-lexer input-port))]
   [(:: #\B #\o #\o #\l)
    ; =>
    (cons (token-BOOLE)
          (minHS-lexer input-port))]
   [(:: #\I #\n #\t)
    ; =>
    (cons (token-INT)
          (minHS-lexer input-port))]
   [(:: #\F #\u #\n #\c)
    ; =>
    (cons (token-FUNC)
          (minHS-lexer input-port))]
   ;; TERMINA relacionado a tipos


   ;; EMPIEZA relacionado a funciones
   [(:: #\f #\u #\n)
    ; =>
    (cons (token-FUN)
          (minHS-lexer input-port))]
   [(:: #\f #\u #\n #\F)
    ; =>
    (cons (token-FUNF)
          (minHS-lexer input-port))]
   [(:: #\= #\>)
    ; =>
    (cons (token-ARROW)
          (minHS-lexer input-port))]
   [(:: #\a #\p #\p)
    ; =>
    (cons (token-APP)
          (minHS-lexer input-port))] 
   ;; TERMINA relacionado a funciones


   ;; EMPIEZA operadores prim
   [#\+
    ; =>
    (cons (token-+)
          (minHS-lexer input-port))]
   [#\-
    ; =>
    (cons (token--)
          (minHS-lexer input-port))]
   [#\*
    ; =>
    (cons (token-*)
          (minHS-lexer input-port))]
   [#\/
    ; =>
    (cons (token-/)
          (minHS-lexer input-port))]

    [(:: #\a #\n #\d)
    ; =>
    (cons (token-AND)
          (minHS-lexer input-port))]
   [(:: #\o #\r)
    ; =>
    (cons (token-OR)
          (minHS-lexer input-port))]
   ;; TERMINA operadores prim
   
   
   ;; EMPIEZA constantes
   [(:+ (char-range #\0 #\9))
    ; =>
    (cons (token-NUM (string->number lexeme))
          (minHS-lexer input-port))]
   [(:: #\# (:or #\t #\f))
    ; =>
    (cons (token-BOOL (equal? lexeme "#t"))
          (minHS-lexer input-port))]
   ;; TERMINA constantes


   ;; Variables
   [(:+ (:: (char-range #\a #\z) (:? (char-range #\0 #\9))))
    ; =>
    (cons (token-VAR (string->symbol lexeme))
          (minHS-lexer input-port))]

   
   ;; EMPIEZA parentesis y brackets
   [#\(
    ;=>
    (cons (token-LP)
          (minHS-lexer input-port))]
   [#\)
    ;=>
    (cons (token-RP)
          (minHS-lexer input-port))]

   [#\{
    ;=>
    (cons (token-LB)
          (minHS-lexer input-port))]
   [#\}
    ;=>
    (cons (token-RB)
          (minHS-lexer input-port))]
   
   [#\[
    ;=>
    (cons (token-LSB)
          (minHS-lexer input-port))]
   [#\]
    ;=>
    (cons (token-RSB)
          (minHS-lexer input-port))]
   ;; TERMINA parentesis y brackets


   ;; Casos especiales
   [whitespace
    ; =>
    (minHS-lexer input-port)] ; borramos todos los posibles espacios en blanco, tabuladores, etc
   [(eof) ; Token que indica que se termino de lexear la cadena
    (list (token-EOF))]))

; Empecemos a definir la gramatica de minHS
; data minHS = NUM Int | ... | ADD minHS minHS | ...
(define-struct let-exp (var e1 e2) #:transparent) ; let(e1,x.e2) --- let x = e1 in e2 end
(define-struct bin-exp (op e1 e2) #:transparent) ; opb(e,e)
(define-struct un-exp (op e1) #:transparent) ; opu(e)
(define-struct par-exp (exp) #:transparent) ; (e)
(define-struct num-exp (n) #:transparent)
(define-struct var-exp (i) #:transparent)
; e :: = num | x | bool | opu(e) | opb(e,e) | fun [(x:T)]* e | ...

; Experimentos bonitos y romanticos
(let* ([input (open-input-file "EjemplitoChido.mhs")]
       [tokens (minHS-lexer input)])
  (begin
    (close-input-port input)
    tokens))

(let ([input (open-input-string "(3 - 33 + 6)")])
  (minHS-lexer input))

(let ([input (open-input-string "fun ([x:Bool][y:Func Bool Bool]:Bool) => if (#t and #t) then {hola} else {adios}")])
  (minHS-lexer input))

; Proximamente un parser
