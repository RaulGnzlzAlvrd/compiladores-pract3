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

(define-tokens a (NUM VAR BOOL TYPE)) ;Tokens que son contenedores --- data Tokens = NUM Int | VAR String | BOOL Bool
(define-empty-tokens b (+ - * / AND OR EOF LET ASSIGN IN LP RP FUN FUNF IF ARROW LSB RSB END)) ;Tokens que no almacenan datos

; "fun (x:T) : T => x"
; [FUN,LP,VAR "x",TYPEOF,TYPE "T",RP,TYPEOF,TYPE "T",ACA,VAR "x",ATA,EOF]
; TYPE (TYPE (TYPE ...))
; T ::= INT | BOOL | T -> T

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
   
   ; :: s*
   [(:: #\l #\e #\t) ; Expresion regular let
    ; =>
    (cons (token-LET)
          (minHS-lexer input-port))] ;token resultante

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

   [(:: #\i #\f)
    ; =>
    (cons (token-IF)
          (minHS-lexer input-port))]

   [(:: #\# (:or #\t #\f))
    ; =>
    (cons (token-BOOL (equal? lexeme "#t"))
          (minHS-lexer input-port))]

   [(:or (:: #\B #\o #\o #\l) (:: #\I #\n #\t) (:: #\F #\u #\n #\c))
    ; =>
    (cons (token-TYPE (string->symbol lexeme))
          (minHS-lexer input-port))]

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
   
   [(:+ (:: (char-range #\a #\z) (:? (char-range #\0 #\9))))
    ;;(:+ (:or (char-range #\a #\z) (char-range #\A #\Z))) ; ([a..z]|[A..Z])^+
    ; =>
    (cons (token-VAR (string->symbol lexeme))
          (minHS-lexer input-port))]

   [(:+ (char-range #\0 #\9))
    ;; (::  (:or #\- (epsilon)) (:: (:* (char-range #\0 #\9)) (:: (:or (:: #\. (char-range #\0 #\9)) (:: (char-range #\0 #\9)) #\.) (:* (char-range #\0 #\9)))))
    ; =>
    (cons (token-NUM (string->number lexeme))
          (minHS-lexer input-port))]
   
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
   [#\(
    ;=>
    (cons (token-LP)
          (minHS-lexer input-port))]

   [#\)
    ;=>
    (cons (token-RP)
          (minHS-lexer input-port))]

   [#\[
    ;=>
    (cons (token-LSB)
          (minHS-lexer input-port))]

   [#\]
    ;=>
    (cons (token-RSB)
          (minHS-lexer input-port))]

   [whitespace ;Caso expecial
    ; =>
    (minHS-lexer input-port)] ;borramos todos los posibles espacios en blanco, tabuladores, etc

   [(eof) ;Token que indica que se termino de lexear la cadena
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
;(let ([input (open-input-string "(3 - 33 + 6)")])
;  (minHS-lexer input))

(let* ([input (open-input-file "EjemplitoChido.mhs")]
       [tokens (minHS-lexer input)])
  (begin
    (close-input-port input)
    tokens))

; Proximamente un parser
