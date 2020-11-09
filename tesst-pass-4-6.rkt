#lang nanopass

(require "parser.rkt"
         "lexer.rkt"
         "ParserNanopass.rkt")

; Tests Ej 4
(rename-var
 (parse-LF
  '(funF f ((z Int))
           Int
           (fun ((y Int)) Int
                (fun ((u Int) (v Int) (w Int)) Int z y u v w)
                (fun ((x Int)) Int x))
           (fun ((x Int)) Int z x))))

; Tests Ej 6
(remove-string
 (parse-LF-CSL
  "foo"))

(remove-string
 (parse-LF-CSL
  '(list #\a 2 "foo")))

(remove-string
 (parse-LF-CSL
  '(list (list #\1 #\2) "12" #\1 #\2)))

(remove-string
 (parse-LF-CSL
  '(if #t "foo" "bar")))
