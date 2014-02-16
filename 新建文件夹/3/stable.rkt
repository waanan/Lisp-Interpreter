#lang eopl

;;old-env
;(define-datatype environment environment?
;  (empty-env)
;  (extend-env
;   (var symbol?)
;   (val expval?)
;   (env environment?))
;  (extend-env-rec
;   (p-names (list-of symbol?))
;   (b-vars (list-of (list-of symbol?)))
;   (body (list-of expression?))
;   (env environment?)))

;(define init-env
;  (lambda ()
;    (extend-env
;     'i (num-val 1)
;     (extend-env
;      'v (num-val 5)
;      (extend-env
;       'x (num-val 10)
;       (empty-env))))))
;(define apply-env
;  (lambda (env search-var)
;    (cases environment env
;      (empty-env ()
;                 (report-no-binding-found search-var))
;      (extend-env (saved-var saved-val saved-env)
;                  (if (eq? saved-var search-var)
;                      saved-val
;                      (apply-env saved-env search-var)))
;      (extend-env-rec (p-names b-varss bodys saved-env)
;                      (apply-env-rec p-names b-varss bodys saved-env search-var env)))))
;(define apply-env-rec 
;  (lambda (p-names b-varss bodys saved-env search-var env)
;    (if (null? p-names)
;        (apply-env env search-var)
;        (let ((name (car p-names))
;              (b-vars (car b-varss))
;              (body (car bodys)))
;         (if (eq? search-var name)
;             (proc-val (procedure b-vars body env))
;             (apply-env-rec (cdr p-names) (cdr b-varss) (cdr bodys) saved-env search-var env))))))


;;new-nameless-env
(define nameless-environment?
  (lambda (x)
    ((list-of (lambda (x) (or (expval? x) (list-of expval?) x))) x)))
(define empty-nameless-env
  (lambda ()
    '()))
(define extend-nameless-env
  (lambda (val nameless-env)
    (cons val nameless-env)))
(define apply-nameless-env
  (lambda (nameless-env n)
    (cond ((not (pair? n))
           (list-ref nameless-env n))
          (else 
           (let ((body (list-ref (list-ref nameless-env (car n)) (cdr n))))
             (proc-val (procedure body (nth nameless-env (car n)))))))))
(define nth 
  (lambda (lst n)
    (if (= n 0)
        lst 
        (nth (cdr lst) (- n 1)))))
             
             
(define list-ref
  (lambda (lst n)
    (if (eq? n 0)
        (car lst)
        (list-ref (cdr lst) (- n 1)))))

(define init-nameless-env
  (lambda ()
    (empty-nameless-env)))

(define empty-senv
  (lambda ()
    '()))
(define extend-senv
  (lambda (var senv)
    (cons var senv)))
(define apply-senv-iter
  (lambda (senv var n)
    (let ((cankao (car senv)))
      (cond ((null? senv)
             (report-no-binding-found var))
            ((and (pair? cankao) (member var cankao))
             (cons n (apply-senv-iter cankao var 0)))
            ((eq? var (car senv))
             n)
            (else 
             (apply-senv-iter (cdr senv) var (+ n 1)))))))
(define apply-senv
  (lambda (senv var)
    (apply-senv-iter senv var 0)))
(define init-senv
  (lambda ()
    (extend-senv 'i
                 (extend-senv 'v
                              (extend-senv 'x
                                           (empty-senv))))))

(define report-no-binding-found
  (lambda (search-var)
    (eopl:error 'apply-env "No binding for ~s" search-var)))
(define report-invalid-env
  (lambda (env)
    (eopl:error 'apply-env "Bad environment: ~s" env)))


;;help-function
(define list-of
  (lambda (pred)
    (lambda (lst)
      (if (null? lst)
          #t
          (and (pair? lst)
               (pred (car lst))
               ((list-of pred) (cdr lst)))))))


;;program define
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
  (nameless-var-exp
   (n (lambda (x) (or (number? x) 
                      (and (number? (car x))
                           (number? (cdr x)))))))
  (let-exp
   (lst1 (list-of symbol?))
   (lst2 (list-of expression?))
   (body expression?))
  (nameless-let-exp
   (exps (list-of expression?))
   (body expression?))
  (nameless-proc-exp
   (body expression?))
  (let*-exp
   (lst1 (list-of symbol?))
   (lst2 (list-of expression?))
   (body expression?))
  (nameless-let*-exp
   (exps (list-of expression?))
   (body expression?))
  (letrec-exp 
   (p-names (list-of symbol?))
   (b-vars (list-of symbol?))
   (bodys (list-of expression?))
   (letrec-body expression?))
  (nameless-letrec-exp
   (bodys (list-of expression?))
   (letrec-body expression?))
  (unpack-exp
   (lst (list-of symbol?))
   (exp1 expression?)
   (exp2 expression?))
  (nameless-unpack-exp
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
   (vars symbol?)
   (exp1 expression?))
  (call-exp
   (rator expression?)
   (rand expression?)))


;;scan-parse
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
     ("letrec" (arbno identifier "(" identifier ")" "=" expression) "in" expression)
     letrec-exp)
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
     ("proc" "(" identifier ")" expression)
     proc-exp)
    (expression
     ("(" expression expression ")")
     call-exp)))

(define just-scan
  (sllgen:make-string-scanner scanner-spec grammar))
(define scan&parse
  (sllgen:make-string-parser scanner-spec grammar))

;;入口
(define run
  (lambda (string)
    (value-of-program 
     (translation-of-program
      (scan&parse string)))))

;;转换为无变量式子
(define translation-of-program
  (lambda (pgm)
    (cases program pgm
      (a-program (exp1)
                 (a-program (translation-of exp1 (init-senv)))))))
;;求值
(define value-of-program
  (lambda (pgm)
    (cases program pgm
      (a-program (exp1)
                 (value-of exp1 (init-nameless-env))))))

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
   (body expression?)
   (saved-nameless-env nameless-environment?)))


;;转换
(define translation-of
  (lambda (exp senv)
    (cases expression exp
      (const1-exp (num) (const1-exp num))
      (const2-exp (bool) (const2-exp bool))
      (var-exp (var) (nameless-var-exp
                      (apply-senv senv var)))
      (construct-exp (exp1 exp2)
                     (construct-exp (translation-of exp1 senv)
                                    (translation-of exp2 senv)))
      (emptylist ()
                 (emptylist))
      (car-exp (exp1)
               (car-exp (translation-of exp1 senv)))
      (cdr-exp (exp1)
               (cdr-exp (translation-of exp1 senv)))
      (list-exp (exps)
                (list-exp (map (lambda (exp) (translation-of exp senv)) exps)))
      (null?-exp (exp1)
                 (null?-exp (translation-of exp1 senv)))
      (diff-exp (exp1 exp2)
                (diff-exp (translation-of exp1 senv)
                          (translation-of exp2 senv)))
      (zero?-exp (exp1)
                 (zero?-exp (translation-of exp1 senv)))
      (if-exp (exp1 exp2 exp3)
              (if-exp (translation-of exp1 senv)
                      (translation-of exp2 senv)
                      (translation-of exp3 senv)))
      (cond-exp (exps1 exps2)
                (cond-exp (map (lambda (x) (translation-of x senv)) exps1) 
                          (map (lambda (x) (translation-of x senv)) exps2)))
      (let-exp (vars exps body)
               (nameless-let-exp
                (map (lambda (exp) (translation-of exp senv)) exps)
                (translation-of body
                                (append (reverse vars) senv))))
      (let*-exp (vars exps body)
                (nameless-let*-exp
                 (trans-let* vars exps senv)
                 (translation-of body
                                (append (reverse vars) senv))))
      ;;ntr
      (letrec-exp (p-names b-vars bodys letrec-body)
                  (nameless-letrec-exp
                   (map (lambda (var body) (translation-of body (extend-senv var (extend-senv p-names senv)))) b-vars bodys)
                   (translation-of letrec-body (extend-senv p-names senv))))
      (unpack-exp (lst exp1 exp2)
                  (nameless-unpack-exp (translation-of exp1 senv)
                                       (translation-of exp2 (append (reverse lst) senv))))
      (minus-exp (exp1)
                 (minus-exp (translation-of exp1 senv)))
      (add-exp (exp1 exp2)
               (add-exp (translation-of exp1 senv)
                        (translation-of exp2 senv)))
      (multi-exp (exp1 exp2)
                 (multi-exp (translation-of exp1 senv)
                            (translation-of exp2 senv)))
      (quoti-exp (exp1 exp2)
                 (quoti-exp (translation-of exp1 senv)
                            (translation-of exp2 senv)))
      (equal?-exp (exp1 exp2)
                  (equal?-exp (translation-of exp1 senv)
                              (translation-of exp2 senv)))
      (greater?-exp (exp1 exp2)
                    (greater?-exp (translation-of exp1 senv)
                                  (translation-of exp2 senv)))
      (less?-exp (exp1 exp2)
                 (less?-exp (translation-of exp1 senv)
                            (translation-of exp2 senv)))
      (proc-exp (var body)
                (nameless-proc-exp
                 (translation-of body
                                 (extend-senv var senv))))
      (call-exp (rator rand)
                (call-exp
                 (translation-of rator senv)
                 (translation-of rand senv)))
      (else (eopl:error 'translation-of "Invalid expression ~s" exp)))))

;;trans-help
(define trans-let*
  (lambda (vars exps senv)
    (if (null? vars)
        '()
        (let ((var (car vars))
              (exp (car exps)))
          (cons (translation-of exp senv)
                (trans-let* (cdr vars) (cdr exps) (extend-senv var senv)))))))

;;求值
(define value-of
  (lambda (exp nameless-env)
    (cases expression exp
      (const1-exp (num) (num-val num))
      (const2-exp (bool) (if (eq? bool 't)
                             (bool-val #t)
                             (bool-val #f)))
      (nameless-var-exp (n) (apply-nameless-env nameless-env n))
      (construct-exp (exp1 exp2)
                     (let ((val1 (value-of exp1 nameless-env))
                           (val2 (value-of exp2 nameless-env)))
                       (cons-exp val1 val2)))
      (emptylist ()
                 (null-exp))
      (car-exp (exp1)
               (let ((val1 (value-of exp1 nameless-env)))
                 (cases lst-exp val1
                   (cons-exp (exp exp2)
                             exp)
                   (else (eopl:error 'car "input are emptylst ~s" exp1)))))
      (cdr-exp (exp1)
               (let ((val1 (value-of exp1 nameless-env)))
                 (cases lst-exp val1
                   (cons-exp (exp exp2)
                             exp2)
                   (else (eopl:error 'car "input are emptylst ~s" exp1)))))
      (list-exp (exps)
                (list->cons exps nameless-env))
      (null?-exp (exp1)
                 (let ((val1 (value-of exp1 nameless-env)))
                   (cases lst-exp val1
                     (null-exp () 
                               (bool-val #t))
                     (cons-exp (exp exp2)
                               (bool-val #f))))) 
      (diff-exp (exp1 exp2)
                (let ((val1 (value-of exp1 nameless-env))
                      (val2 (value-of exp2 nameless-env)))
                  (let ((num1 (expval->num val1))
                        (num2 (expval->num val2)))
                    (num-val
                     (- num1 num2)))))
      (zero?-exp (exp1)
                 (let ((val1 (value-of exp1 nameless-env)))
                   (let ((num1 (expval->num val1)))
                     (if (= num1 0)
                         (bool-val #t)
                         (bool-val #f)))))
      
      (if-exp (exp1 exp2 exp3)
              (let ((val1 (value-of exp1 nameless-env)))
                (if (expval->bool val1)
                    (value-of exp2 nameless-env)
                    (value-of exp3 nameless-env))))
      (cond-exp (exps1 exps2)
                (cond->val exps1 exps2 nameless-env))
      (nameless-let-exp (exps body)
                        (let->val exps body nameless-env nameless-env))
      (nameless-let*-exp (exps body)
                (let*->val exps body nameless-env))
      
      (nameless-letrec-exp (bodys letrec-body)
                  (value-of letrec-body (extend-nameless-env bodys nameless-env)))
      (nameless-unpack-exp (exp1 exp2)
                  (nameless-unpack->val exp1 exp2 nameless-env))
      (minus-exp (exp1)
                 (let ((val1 (value-of exp1 nameless-env)))
                   (let ((num1 (expval->num val1)))
                     (num-val (- num1)))))
      (add-exp (exp1 exp2)
               (let ((val1 (value-of exp1 nameless-env))
                     (val2 (value-of exp2 nameless-env)))
                 (let ((num1 (expval->num val1))
                       (num2 (expval->num val2)))
                   (num-val
                    (+ num1 num2)))))
      (multi-exp (exp1 exp2)
                 (let ((val1 (value-of exp1 nameless-env))
                       (val2 (value-of exp2 nameless-env)))
                   (let ((num1 (expval->num val1))
                         (num2 (expval->num val2)))
                     (num-val
                      (* num1 num2)))))
      (quoti-exp (exp1 exp2)
                 (let ((val1 (value-of exp1 nameless-env))
                       (val2 (value-of exp2 nameless-env)))
                   (let ((num1 (expval->num val1))
                         (num2 (expval->num val2)))
                     (num-val
                      (quotient num1 num2)))))
      (equal?-exp (exp1 exp2)
                  (let ((val1 (value-of exp1 nameless-env))
                        (val2 (value-of exp2 nameless-env)))
                    (let ((num1 (expval->num val1))
                          (num2 (expval->num val2)))
                      (bool-val
                       (= num1 num2)))))
      (greater?-exp (exp1 exp2)
                    (let ((val1 (value-of exp1 nameless-env))
                          (val2 (value-of exp2 nameless-env)))
                      (let ((num1 (expval->num val1))
                            (num2 (expval->num val2)))
                        (bool-val
                         (> num1 num2)))))
      (less?-exp (exp1 exp2)
                 (let ((val1 (value-of exp1 nameless-env))
                       (val2 (value-of exp2 nameless-env)))
                   (let ((num1 (expval->num val1))
                         (num2 (expval->num val2)))
                     (bool-val
                      (< num1 num2)))))
      (nameless-proc-exp (body)
                         (proc-val (procedure body nameless-env)))
      (call-exp (rator rand)
                (let ((proc (expval->proc (value-of rator nameless-env))))
                  (apply-procedure proc (value-of rand nameless-env))))
      (else (eopl:error 'value-of "Invalid value ~s in ~s" exp nameless-env)))))


;;value-of help func
(define list->cons
  (lambda (exps nameless-env)
    (if (null? exps)
        (null-exp)
        (cons-exp (value-of (car exps) nameless-env)
                  (list->cons (cdr exps) nameless-env)))))
(define cond->val
  (lambda (exps1 exps2 nameless-env)
    (if (null? exps1)
        (eopl:error 'cond "condition has no element ~s ~s" exps1 exps2)
        (let ((elem1 (car exps1))
              (elem2 (car exps2))
              (rest1 (cdr exps1))
              (rest2 (cdr exps2)))
          (let ((tiaojian (value-of elem1 nameless-env))
                (jieguo (value-of elem2 nameless-env)))
            (if (expval->bool tiaojian)
                jieguo
                (cond->val rest1 rest2 nameless-env)))))))
(define let->val
  (lambda (exps body nameless-env new-env)
    (if (null? exps)
        (value-of body new-env)
        (let ((val (value-of (car exps) nameless-env)))
          (let->val (cdr exps) body nameless-env (extend-nameless-env val new-env))))))
(define let*->val
  (lambda (exps body nameless-env)
    (if (null? exps)
        (value-of body nameless-env)
        (let ((val (value-of (car exps) nameless-env)))
          (let*->val (cdr exps) body (extend-nameless-env val nameless-env))))))
(define nameless-unpack->val
  (lambda (exp1 exp2 nameless-env)
    (cond ((expval->bool (value-of (null?-exp exp1) nameless-env)) (value-of exp2 nameless-env))
          (else (let ((val (value-of (car-exp exp1) nameless-env)))
                  (nameless-unpack->val (cdr-exp exp1) exp2 (extend-nameless-env val nameless-env))))))) 

(define apply-procedure
  (lambda (proc1 val)
    (cases proc proc1
      (procedure (body saved-nameless-env)
                 (value-of body
                           (extend-nameless-env val saved-nameless-env))))))

;(define apply-procedure
;  (lambda (proc1 rands now-env)
;    (cases proc proc1
;      (procedure (vars body saved-env)
;                     (apply-help
;                      vars rands  body saved-env now-env)))))
;(define apply-help
;  (lambda (vars rands body saved-env now-env)
;    (cond ((and (null? vars) (not (null? rands)))
;           (eopl:error 'proc "rands 多于 rator"))
;          ((and (null? rands) (not (null? vars)))
;          (eopl:error 'proc "args not enough"))
;         ((and (null? rands) (null? vars))
;         (value-of body saved-env))
;       (else
;       (let ((var (car vars))
;            (arg (value-of (car rands) now-env))
;           (rest-vars (cdr vars))
;          (rest-rands (cdr rands)))
;     (apply-help rest-vars rest-rands body (extend-env var arg saved-env) now-env))))))
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















