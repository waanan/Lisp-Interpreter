#lang eopl
;;example  (scan&parse "{x := foo; while x do x := (x - bar)}")
;(define-datatype statement statement?
;  (compound-statement
;   (stmt1 statement?)
;   (stmt2 statement?))
;  (while-statement
;   (test expression?)
;   (body statement?))
;  (assign-statement
;   (lhs symbol?)
;   (rhs expression?)))
;(define-datatype expression expression?
;  (var-exp
;   (var symbol?))
;  (diff-exp
;   (exp1 expression?)
;   (exp2 expression?)))
;(define scanner-spec-1
;  '((white-sp (whitespace) skip)
;    (comment ("%" (arbno (not #\newline))) skip)
;    (identifier (letter (arbno (or letter digit))) symbol)
;    (number (digit (arbno digit)) number)))
;(define grammar-1
;  '((statement
;     ("{" statement ";" statement "}")
;     compound-statement)
;    (statement
;     ("while" expression "do" statement)
;     while-statement)
;    (statement
;     (identifier ":=" expression)
;     assign-statement)
;    (expression
;     (identifier)
;     var-exp)
;    (expression
;     ("(" expression "-" expression ")")
;     diff-exp)))
;(define just-scan
;  (sllgen:make-string-scanner scanner-spec-1 grammar-1))
;(define scan&parse
;  (sllgen:make-string-parser scanner-spec-1 grammar-1))
           

(define-datatype program program?
  (a-program
   (exp1 expression?)))
(define-datatype expression expression?
  (const-exp
   (num number?))
  (diff-exp
   (exp1 expression?)
   (exp2 expression?))
  (zero?-exp
   (exp1 expression?))
  (if-exp
   (exp1 expression?)
   (exp2 expression?)
   (exp3 expression?))
  (var-exp
   (var symbol?))
  (let-exp
   (var symbol?)
   (exp1 expression?)
   (body expression?)))
(define scanner-spec
  '((white-sp (whitespace) skip)
    (comment ("%" (arbno (not #\newline))) skip)
    (identifier (letter (arbno (or letter digit))) symbol)
    (number (digit (arbno digit)) number)))
(define grammar
  '((program
     (expression)
     a-program)
    (expression
     (number)
     const-exp)
    (expression
     ("-" "(" expression "," expression ")")
     diff-exp)
    (expression
     (identifier)
     var-exp)
    (expression
     ("zero?" expression)
     zero?-exp)
    (expression
     ("if" "(" expression ")" expression "else" expression)
     if-exp)
    (expression
     ("let" identifier "=" expression "in" expression)
    let-exp)))
(define just-scan
  (sllgen:make-string-scanner scanner-spec grammar))
(define scan&parse
  (sllgen:make-string-parser scanner-spec grammar))








