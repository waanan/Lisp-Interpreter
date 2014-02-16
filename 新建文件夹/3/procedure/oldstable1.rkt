#lang eopl
(define empty-env
  (lambda () (list 'empty-env)))
(define extend-env
  (lambda (var val env)
    (list 'extend-env var val env)))
(define environment?
  (lambda (env)
  (or (eq? (car env) 'empty-env)
      (eq? (car env) 'extend-env))))

(define init-env
  (lambda ()
    (extend-env
     'i (num-val 1)
     (extend-env
      'v (num-val 5)
      (extend-env
       'x (num-val 10)
       (empty-env))))))

(define apply-env
  (lambda (env search-var)
    (cond
      ((eqv? (car env) 'empty-env)
       (report-no-binding-found search-var))
      ((eqv? (car env) 'extend-env)
       (let ((saved-var (cadr env))
             (saved-val (caddr env))
             (saved-env (cadddr env)))
         (if (eqv? search-var saved-var)
             saved-val
             (apply-env saved-env search-var))))
      (else
       (report-invalid-env env)))))
(define report-no-binding-found
  (lambda (search-var)
    (eopl:error 'apply-env "No binding for ~s" search-var)))
(define report-invalid-env
  (lambda (env)
    (eopl:error 'apply-env "Bad environment: ~s" env)))

(define list-of
  (lambda (pred)
    (lambda (lst)
      (if (null? lst)
          #t
          (and (pair? lst)
               (pred (car lst))
               ((list-of pred) (cdr lst)))))))

(define-datatype program program?
  (a-program
   (exp1 expression?)))
(define-datatype expression expression?
  (const1-exp
   (num number?))
  (const2-exp
   (bool symbol?))
  (emptylist)
  (construct-exp
   (exp1 expression?)
   (exp2 expression?))
  (car-exp
   (exp1 expression?))
  (cdr-exp
   (exp1 expression?))
  (null?-exp
   (exp1 expression?))
  (list-exp
   (exps (list-of expression?)))
  (diff-exp
   (exp1 expression?)
   (exp2 expression?))
  (zero?-exp
   (exp1 expression?))
  (if-exp
   (exp1 expression?)
   (exp2 expression?)
   (exp3 expression?))
  (cond-exp
   (exps1 (list-of expression?))
   (exps2 (list-of expression?)))
  (var-exp
   (var symbol?))
  (let-exp
   (lst1 (list-of symbol?))
   (lst2 (list-of expression?))
   (body expression?))
  (let*-exp
   (lst1 (list-of symbol?))
   (lst2 (list-of expression?))
   (body expression?))
  (unpack-exp
   (lst (list-of symbol?))
   (exp1 expression?)
   (exp2 expression?))
  (minus-exp
   (exp1 expression?))
  (add-exp
   (exp1 expression?)
   (exp2 expression?))
  (multi-exp
   (exp1 expression?)
   (exp2 expression?))  
  (quoti-exp
   (exp1 expression?)
   (exp2 expression?))
  (equal?-exp
   (exp1 expression?)
   (exp2 expression?))
  (less?-exp
   (exp1 expression?)
   (exp2 expression?))
  (greater?-exp
   (exp1 expression?)
   (exp2 expression?))
  (proc-exp 
   (vars (list-of symbol?))
   (exp1 expression?))
  (call-exp
   (rator expression?)
   (rands (list-of expression?))))
    
(define scanner-spec
  '((white-sp (whitespace) skip)
    (comment (";" (arbno (not #\newline))) skip)
    (identifier (letter (arbno (or letter digit))) symbol)
;    (boolean ("#" (or "t" "f")) symbol) 
    (number ((or "" "-") digit (arbno digit)) number)))
(define grammar
  '((program
     (expression)
     a-program)
    (expression
     (number)
     const1-exp)
    (expression
     ("#" identifier)
     const2-exp)
    (expression
     ("-" "(" expression "," expression ")")
     diff-exp)
    (expression
     (identifier)
     var-exp)
    (expression
     ("zero?" "(" expression ")")
     zero?-exp)
    (expression
     ("if" expression "then" expression "else" expression)
     if-exp)
    (expression
     ("cond" (arbno expression "==>" expression) "end")
     cond-exp)
    (expression
     ("let" (arbno identifier "=" expression) "in" expression)
    let-exp)
    (expression
     ("let*" (arbno identifier "=" expression) "in" expression)
     let*-exp)
    (expression
     ("unpack" (arbno identifier) "=" expression "in" expression)
     unpack-exp)
    (expression
     ("minus" "(" expression ")")
     minus-exp)
    (expression
     ("+" "(" expression "," expression ")")
     add-exp)
    (expression
     ("*" "(" expression "," expression ")")
     multi-exp)
    (expression
     ("/" "(" expression "," expression ")")
     quoti-exp)
    (expression
     ("=?" "(" expression "," expression ")")
     equal?-exp)
    (expression
     (">?" "(" expression "," expression ")")
     greater?-exp)
    (expression
     ("<?" "(" expression "," expression ")")
     less?-exp)
    (expression
     ("emptylist")
     emptylist)
    (expression
     ("cons" "(" expression "," expression ")")
     construct-exp)
    (expression
     ("car" "(" expression ")")
     car-exp)
    (expression
     ("cdr" "(" expression ")")
     cdr-exp)
    (expression
     ("list" "(" (separated-list expression ",") ")")
     list-exp)
    (expression
     ("null?" "(" expression ")")
     null?-exp)
    (expression
     ("proc" "(" (separated-list identifier ",") ")" expression)
     proc-exp)
    (expression
     ("(" expression (arbno expression) ")")
     call-exp)))
(define just-scan
  (sllgen:make-string-scanner scanner-spec grammar))
(define scan&parse
  (sllgen:make-string-parser scanner-spec grammar))

;;基本数据类型
(define-datatype expval expval?
  (num-val
   (num number?))
  (bool-val
   (bool boolean?))
  (proc-val
   (proc proc?)))
(define expval->num
  (lambda (val)
    (cases expval val
      (num-val (num) num)
      (else (report-expval-extractor-error 'num val))))) 
(define expval->bool
  (lambda (val)
    (cases expval val
      (bool-val (bool) bool)
      (else (report-expval-extractor-error 'bool val)))))
(define expval->proc
  (lambda (val)
    (cases expval val
      (proc-val (proc) proc)
      (else (report-expval-extractor-error 'proc val)))))
(define cons-accept?
  (lambda (x)
    (or (lst-exp? x)
        (expval? x))))          
(define-datatype lst-exp lst-exp?
  (null-exp)
  (cons-exp
   (first cons-accept?)
   (second cons-accept?)))
(define report-expval-extractor-error 
  (lambda (x y)
    (eopl:error x "~s extract error" y)))

(define-datatype proc proc?
  (procedure
   (vars (list-of symbol?))
   (body expression?)
   (saved-env environment?)))

(define run
  (lambda (string)
    (value-of-program (scan&parse string))))
(define value-of-program
  (lambda (pgm)
    (cases program pgm
      (a-program (exp1)
                 (value-of exp1 (init-env))))))
(define value-of
  (lambda (exp env)
    (cases expression exp
      (const1-exp (num) (num-val num))
      (const2-exp (bool) (if (eq? bool 't)
                             (bool-val #t)
                             (bool-val #f)))
      (var-exp (var) (apply-env env var))
      (construct-exp (exp1 exp2)
                (let ((val1 (value-of exp1 env))
                      (val2 (value-of exp2 env)))
                  (cons-exp val1 val2)))
      (emptylist ()
                 (null-exp))
      (car-exp (exp1)
               (let ((val1 (value-of exp1 env)))
                 (cases lst-exp val1
                   (cons-exp (exp exp2)
                             exp)
                   (else (eopl:error 'car "input are emptylst ~s" exp1)))))
      (cdr-exp (exp1)
               (let ((val1 (value-of exp1 env)))
                 (cases lst-exp val1
                   (cons-exp (exp exp2)
                             exp2)
                   (else (eopl:error 'car "input are emptylst ~s" exp1)))))
      (list-exp (exps)
                (list->cons exps env))
      (null?-exp (exp1)
                 (let ((val1 (value-of exp1 env)))
                   (cases lst-exp val1
                     (null-exp () 
                               (bool-val #t))
                     (cons-exp (exp exp2)
                               (bool-val #f))))) 
      (diff-exp (exp1 exp2)
                (let ((val1 (value-of exp1 env))
                      (val2 (value-of exp2 env)))
                  (let ((num1 (expval->num val1))
                        (num2 (expval->num val2)))
                    (num-val
                     (- num1 num2)))))
      (zero?-exp (exp1)
                 (let ((val1 (value-of exp1 env)))
                   (let ((num1 (expval->num val1)))
                     (if (= num1 0)
                         (bool-val #t)
                         (bool-val #f)))))
      (if-exp (exp1 exp2 exp3)
              (let ((val1 (value-of exp1 env)))
                (if (expval->bool val1)
                    (value-of exp2 env)
                    (value-of exp3 env))))
      (cond-exp (exps1 exps2)
                (cond->val exps1 exps2 env))
      (let-exp (lst1 lst2 body)
               (let->val lst1 lst2 body env env))
      (let*-exp (lst1 lst2 body)
               (let*->val lst1 lst2 body env))
      (unpack-exp (lst exp1 exp2)
                  (unpack->val lst exp1 exp2 env))
      (minus-exp (exp1)
                 (let ((val1 (value-of exp1 env)))
                   (let ((num1 (expval->num val1)))
                     (num-val (- num1)))))
      (add-exp (exp1 exp2)
                (let ((val1 (value-of exp1 env))
                      (val2 (value-of exp2 env)))
                  (let ((num1 (expval->num val1))
                        (num2 (expval->num val2)))
                    (num-val
                     (+ num1 num2)))))
      (multi-exp (exp1 exp2)
                (let ((val1 (value-of exp1 env))
                      (val2 (value-of exp2 env)))
                  (let ((num1 (expval->num val1))
                        (num2 (expval->num val2)))
                    (num-val
                     (* num1 num2)))))
      (quoti-exp (exp1 exp2)
                (let ((val1 (value-of exp1 env))
                      (val2 (value-of exp2 env)))
                  (let ((num1 (expval->num val1))
                        (num2 (expval->num val2)))
                    (num-val
                     (quotient num1 num2)))))
      (equal?-exp (exp1 exp2)
                (let ((val1 (value-of exp1 env))
                      (val2 (value-of exp2 env)))
                  (let ((num1 (expval->num val1))
                        (num2 (expval->num val2)))
                    (bool-val
                     (= num1 num2)))))
      (greater?-exp (exp1 exp2)
                (let ((val1 (value-of exp1 env))
                      (val2 (value-of exp2 env)))
                  (let ((num1 (expval->num val1))
                        (num2 (expval->num val2)))
                    (bool-val
                     (> num1 num2)))))
      (less?-exp (exp1 exp2)
                (let ((val1 (value-of exp1 env))
                      (val2 (value-of exp2 env)))
                  (let ((num1 (expval->num val1))
                        (num2 (expval->num val2)))
                    (bool-val
                     (< num1 num2)))))
      (proc-exp (vars body)
                (proc-val (procedure vars body env)))
      (call-exp (rator rands)
                (let ((proc (expval->proc (value-of rator env))))
                  (apply-procedure proc rands env))))))
                  
                
;;value-of help func
(define list->cons
  (lambda (exps env)
    (if (null? exps)
        (null-exp)
        (cons-exp (value-of (car exps) env)
              (list->cons (cdr exps) env)))))
(define cond->val
  (lambda (exps1 exps2 env)
    (if (null? exps1)
        (eopl:error 'cond "condition has no element ~s ~s" exps1 exps2)
        (let ((elem1 (car exps1))
              (elem2 (car exps2))
              (rest1 (cdr exps1))
              (rest2 (cdr exps2)))
          (let ((tiaojian (value-of elem1 env))
                (jieguo (value-of elem2 env)))
            (if (expval->bool tiaojian)
                jieguo
                (cond->val rest1 rest2 env)))))))
(define let->val
  (lambda (lst1 lst2 body env-old env-new)
    (if (null? lst1)
        (value-of body env-new)
        (let ((var (car lst1))
              (val (value-of (car lst2) env-old)))
          (let->val (cdr lst1) (cdr lst2) body env-old (extend-env var val env-new))))))
(define let*->val
  (lambda (lst1 lst2 body env)
    (if (null? lst1)
        (value-of body env)
        (let ((var (car lst1))
              (val (value-of (car lst2) env)))
          (let*->val (cdr lst1) (cdr lst2) body (extend-env var val env))))))
(define unpack->val
  (lambda (lst exp1 exp2 env)
    (cond ((and (null? lst) (expval->bool (value-of (null?-exp exp1) env))) (value-of exp2 env))
          ((and (null? lst) (not (expval->bool (value-of (null?-exp exp1) env)))) (eopl:error 'unpack "vars and list not match"))
          (else (let ((var (car lst))
                      (val (value-of (car-exp exp1) env)))
                  (unpack->val (cdr lst) (cdr-exp exp1) exp2 (extend-env var val env))))))) 
(define apply-procedure
  (lambda (proc1 rands now-env)
    (cases proc proc1
      (procedure (vars body saved-env)
                     (apply-help
                      vars rands  body saved-env now-env)))))
(define apply-help
  (lambda (vars rands body saved-env now-env)
    (cond ((and (null? vars) (not (null? rands)))
           (eopl:error 'proc "rands 多于 rator"))
          ((and (null? rands) (not (null? vars)))
           (eopl:error 'proc "args not enough"))
          ((and (null? rands) (null? vars))
           (value-of body saved-env))
          (else
           (let ((var (car vars))
                 (arg (value-of (car rands) now-env))
                 (rest-vars (cdr vars))
                 (rest-rands (cdr rands)))
             (apply-help rest-vars rest-rands body (extend-env var arg saved-env) now-env)))))) 
;;exercise3.23 fact
(define fact
  "let facthelper = proc (facthelp)
                         proc (n,rec)
                         if   =?(n,1)
                         then     rec
                         else ((facthelp facthelp) -(n,1) *(rec,n))
       in let fact = proc (n) ((facthelper facthelper) n 1)
          in (fact 4)")
;;exercise3.24 even? and odd?
(define even?-odd?
  "let helper = proc (help)
                     proc (n,panding)
                     if     =?(n,0)
                     then   if =?(panding,0)
                            then #t
                            else #f
                     else   if =?(panding,0)
                            then ((help help) -(n,1) 1)
                            else ((help help) -(n,1) 0)
      in let even = proc (n) ((helper helper) n 0)
             odd  = proc (n) ((helper helper) n 1)
             in (odd 3)")
            









  
  
  
  
  
