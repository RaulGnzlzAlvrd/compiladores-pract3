#lang nanopass

(require "lexer.rkt"
         "parser.rkt"
         (prefix-in : parser-tools/lex-sre)
         (prefix-in re- parser-tools/lex-sre)
         parser-tools/yacc
         parser-tools/lex)

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

(display "Example 1: 3 - (3 / 6)\n")
(let ((input (open-input-string " (2 + 3))")))
  (minHS-parser (lex-this minHS-lexer input)))
