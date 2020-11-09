#lang nanopass

(require "parser.rkt"
         "lexer.rkt"
         "ParserNanopass.rkt")

; Concrete expression;
; (33 + 2)
(expr->string (par-exp (prim-exp + (num-exp 33) (num-exp 2))))
; Answer: "(+ 33 2)"

; Concrete expression
; 3 - (3 / 6)
(expr->string (prim-exp - (num-exp 3) 
    (par-exp (prim-exp - (num-exp 3) (num-exp 6)))))
; Answer "(- 3 (/ 3 6))"

; Concrete expression:
; if(#t and #f)then{2}else{3}
(expr->string (if-then-else-exp (prim-exp 'and (bool-exp #t) (bool-exp #f))
    (num-exp 2) (num-exp 3)))
; Answer: "(if (and #t #f) 2 3)"

; Concrete expression:
; fun ([x:Int]:Int) => x
(expr->string (fun-exp (typeof-exp (brack-exp (typeof-exp (var-exp 'x)
    (int-exp))) (int-exp)) (var-exp 'x)))
; Answer: "(fun ([x Int]) Int x)"

; Concrete expression:
; fun ([x:Int][y:Int]:Int) => x*y
(expr->string (fun-exp
 (typeof-exp (brack-exp (app-t-exp 
    (typeof-exp (var-exp 'x) 
        (int-exp)) (typeof-exp (var-exp 'y) (int-exp)))) (int-exp))
 (prim-exp * (var-exp 'x) (var-exp 'y))))
; Answer: "(fun ([x Int] [y Int]) Int (* x y))"

; Concrete expression:
; fun ([x:Int][y:Int][z:Int][a:Int]:Int) => x*y
(expr->string (fun-exp
 (typeof-exp
  (brack-exp
   (app-t-exp
    (app-t-exp
     (app-t-exp
      (typeof-exp (var-exp 'x) (int-exp))
      (typeof-exp (var-exp 'y) (int-exp)))
     (typeof-exp (var-exp 'z) (int-exp)))
    (typeof-exp (var-exp 'a) (int-exp))))
  (int-exp))
 (prim-exp + (var-exp 'x) (var-exp 'y))))
; Answer: "(fun ([x Int] [y Int] [z Int] [a Int]) Int (+ x y))"

; Concrete expression:
; funF (sumita ([x:Int][y:Int]):Int) => x+y
(expr->string (let ((input (open-input-string "funF (sumita ([x:Int][y:Int]):Int) => x+y")))
  (minHS-parser (lex-this minHS-lexer input))))
; Answer: "(funF sumita ([x Int] [y Int]) Int (+ x y))"

; Concrete expression:
; let ([x:Int = 1][y:Int = 2][3:Int = 3]) in x+y*z end
(expr->string (let ((input (open-input-string "let ([x:Int = 1][y:Int = 2][3:Int = 3]) in x+y*z end")))
  (minHS-parser (lex-this minHS-lexer input))))
; Answer: "(let ([x Int = 1] [y Int = 2] [3 Int = 3]) (+ x (* y z)))"