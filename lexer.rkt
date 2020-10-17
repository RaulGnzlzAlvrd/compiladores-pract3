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
                          LP RP LB RB LK RK   ;; parentesis (), brackets {} y square brackets []
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
    (token-LET)]
   [#\=
    ; =>
    (token-ASSIGN)]
   [(:: #\i #\n)
    ; =>
    (token-IN)]
   [(:: #\e #\n #\d)
    ; =>
    (token-END)]
   ;; TERMINA relacionado a let
   

   ;; EMPIEZA relacionado al if
   [(:: #\i #\f)
    ; =>
    (token-IF)]
   [(:: #\t #\h #\e #\n)
    ; =>
    (token-THEN)]
   [(:: #\e #\l #\s #\e)
    ; =>
    (token-ELSE)]
   ;; TERMINA relacionado al if

   
   ;; EMPIEZA relacionado a tipos
   [#\:
    ; =>
    (token-TYPEOF)]
   [(:: #\B #\o #\o #\l)
    ; =>
    (token-BOOLE)]
   [(:: #\I #\n #\t)
    ; =>
    (token-INT)]
   [(:: #\F #\u #\n #\c)
    ; =>
    (token-FUNC)]
   ;; TERMINA relacionado a tipos


   ;; EMPIEZA relacionado a funciones
   [(:: #\f #\u #\n)
    ; =>
    (token-FUN)]
   [(:: #\f #\u #\n #\F)
    ; =>
    (token-FUNF)]
   [(:: #\= #\>)
    ; =>
    (token-ARROW)]
   [(:: #\a #\p #\p)
    ; =>
    (token-APP)] 
   ;; TERMINA relacionado a funciones


   ;; EMPIEZA operadores prim
   [#\+
    ; =>
    (token-+)]
   [#\-
    ; =>
    (token--)]
   [#\*
    ; =>
    (token-*)]
   [#\/
    ; =>
    (token-/)]
   [(:: #\a #\n #\d)
    ; =>
    (token-AND)]
   [(:: #\o #\r)
    ; =>
    (token-OR)]
   ;; TERMINA operadores prim
   
   
   ;; EMPIEZA constantes
   [(:: (:? (:+ #\- #\+)) (:+ (char-range #\0 #\9)))
    ; =>
    (token-NUM (string->number lexeme))]
   [(:: #\# (:or #\t #\f))
    ; =>
    (token-BOOL (equal? lexeme "#t"))]
   ;; TERMINA constantes


   ;; Variables
   [(:+ (:: (char-range #\a #\z) (:? (char-range #\0 #\9))))
    ; =>
    (token-VAR (string->symbol lexeme))]

   
   ;; EMPIEZA parentesis y brackets
   [#\(
    ;=>
    (token-LP)]
   [#\)
    ;=>
    (token-RP)]
   [#\{
    ;=>
    (token-LK)]
   [#\}
    ;=>
    (token-RK)]
   [#\[
    ;=>
    (token-LB)]
   [#\]
    ;=>
    (token-RB)]
   ;; TERMINA parentesis y brackets


   ;; Casos especiales
   [whitespace
    ; =>
    (minHS-lexer input-port)] ; borramos todos los posibles espacios en blanco, tabuladores, etc
   [(eof) ; Token que indica que se termino de lexear la cadena
    (token-EOF)]))

; Empecemos a definir la gramatica de minHS
; data minHS = NUM Int | ... | ADD minHS minHS | ...
(define-struct let-exp (var e1 e2) #:transparent) ; let(e1,x.e2) --- let x = e1 in e2 end
(define-struct bin-exp (op e1 e2) #:transparent) ; opb(e,e)
(define-struct un-exp (op e1) #:transparent) ; opu(e)
(define-struct par-exp (exp) #:transparent) ; (e)
(define-struct num-exp (n) #:transparent)
(define-struct var-exp (i) #:transparent)
; e :: = num | x | bool | opu(e) | opb(e,e) | fun [(x:T)]* e | ...

;; Experimentos bonitos y romanticos
;; Casi todos los ejemplos chidos están en este archivo
;; Para que salga más bonito se puede revertir a al commit e4f6d20
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
