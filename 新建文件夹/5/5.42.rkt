#lang eopl
(define-datatype environment environment?
  (empty-env)
  (extend-env
   (var symbol?)
   (val (lambda (x) (or (expval? x) (continuation? x))))
   (env environment?))
  (extend-env-rec
   (p-name symbol?)
   (b-vars (list-of symbol?))
   (body expression?)
   (env environment?)))

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
    (cases environment env
      (empty-env ()
                 (report-no-binding-found search-var))
      (extend-env (saved-var saved-val saved-env)
                  (if (eq? saved-var search-var)
                      saved-val
                      (apply-env saved-env search-var)))
      (extend-env-rec (p-name b-vars body saved-env)
                      (if (eq? search-var p-name)
                          (proc-val (procedure b-vars body env))
                          (apply-env saved-env search-var))))))
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
   (exp expression?)
   (body expression?))
  (letrec-exp 
   (p-name symbol?)
   (b-vars (list-of symbol?))
   (body expression?)
   (letrec-body expression?))
  (letcc-exp
   (var symbol?)
   (body expression?))
  (throw-exp
   (exp1 expression?)
   (exp2 expression?))
  (try-exp
   (exp1 expression?)
   (var symbol?)
   (exp2 expression?))
  (raise-exp
   (exp1 expression?))
  (proc-exp 
   (vars symbol?)
   (exp1 expression?))
  (call-exp
   (rator expression?)
   (rands expression?)))

(define-datatype continuation continuation?
  (end-cont)
  (zero1-cont
   (cont continuation?))
  (let-exp-cont
   (var symbol?)
   (body expression?)
   (env environment?)
   (cont continuation?))
  (throw-cont
   (exp2 expression?)
   (env environment?)
   (cont continuation?))
  (throw2-cont
   (val expval?))
  (if-test-cont
   (exp2 expression?)
   (exp3 expression?)
   (env environment?)
   (cont continuation?))
  (diff1-cont
   (exp2 expression?)
   (env environment?)
   (cont continuation?))
  (diff2-cont
   (val1 expval?)
   (cont continuation?))
  (try-cont
   (var symbol?)
   (exp expression?)
   (env environment?)
   (cont continuation?))
  (raise-cont
   (cont continuation?))
  (rator-cont
   (rand expression?)
   (env environment?)
   (cont continuation?))
  (rand-cont
   (val1 expval?)
   (cont continuation?)))
(define apply-cont
  (lambda (cont val)
    (cases continuation cont
      (end-cont ()
                (begin 
                  (eopl:printf "End of computation.~%")
                  val))
      (zero1-cont (saved-cont)
                  (apply-cont saved-cont 
                              (bool-val (= (expval->num val) 0))))
      (let-exp-cont (var body saved-env saved-cont)
                    (value-of/k body
                                (extend-env var val saved-env)
                                saved-cont))
      (throw-cont (exp2 env cont)
                  (value-of/k exp2 env (throw2-cont val)))
      (throw2-cont (val1)
                   (apply-cont val val1))
      (if-test-cont (exp2 exp3 saved-env saved-cont)
                    (if (expval->bool val)
                        (value-of/k exp2 saved-env saved-cont)
                        (value-of/k exp3 saved-env saved-cont) ))
      (diff1-cont (exp2 env cont)
                  (value-of/k exp2 env (diff2-cont val cont)))
      (diff2-cont (val1 cont)
                  (let ((num1 (expval->num val1))
                        (num2 (expval->num val)))
                    (apply-cont cont
                                (num-val (- num1 num2)))))
      (try-cont (var exp env cont)
                (apply-cont cont val))
      (raise-cont (cont)
                  (apply-handler val cont))
      (rator-cont (rand env cont)
                  (value-of/k rand env
                            (rand-cont val cont)))
      (rand-cont (val1 cont)
                 (let ((proc1 (expval->proc val1)))
                   (apply-procedure/k proc1 val cont))))))
(define apply-handler
  (lambda (val cont)
    (cases continuation cont
      (end-cont ()
                (eopl:error "Uncaught exception!"))
      (try-cont (var exp env cont)
                (value-of/k exp (extend-env var val env) cont))
      (zero1-cont (saved-cont)
                  (apply-handler val saved-cont))
      (let-exp-cont (var body saved-env saved-cont)
                    (apply-handler val saved-cont))
      (if-test-cont (exp2 exp3 saved-env saved-cont)
                    (apply-handler val saved-cont))
      (diff1-cont (exp2 env saved-cont)
                 (apply-handler val saved-cont))
      (diff2-cont (val1 saved-cont)
                 (apply-handler val saved-cont))
      (raise-cont (saved-cont)
                  (apply-handler val saved-cont))
      (rator-cont (rand env saved-cont)
                  (apply-handler val saved-cont))
      (rand-cont (val1 saved-cont)
                 (apply-handler val saved-cont))
      (else (eopl:error "raise-error")))))
                

    
(define scanner-spec
  '((white-sp (whitespace) skip)
    (comment ("%" (arbno (not #\newline))) skip)
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
     ("let" identifier "=" expression "in" expression)
     let-exp)
    (expression
     ("letrec" identifier "(" (arbno identifier) ")" "=" expression "in" expression)
     letrec-exp)
    (expression
     ("letcc" identifier expression)
     letcc-exp)
    (expression
     ("throw" expression "to" expression)
     throw-exp)
    (expression
     ("try" expression "catch" "(" identifier ")" expression)
     try-exp)
    (expression
     ("raise" expression)
     raise-exp)
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
                 (value-of/k exp1 (init-env) (end-cont))))))
(define value-of/k
  (lambda (exp env cont)
    (cases expression exp
      (const1-exp (num) (apply-cont cont (num-val num)))
      (const2-exp (bool) (apply-cont cont
                                     (if (eq? bool 't)
                                         (bool-val #t)
                                         (bool-val #f))))
      (var-exp (var) (apply-cont cont (apply-env env var)))
      (diff-exp (exp1 exp2)
                (value-of/k exp1 env 
                          (diff1-cont exp2 env cont)))
      (zero?-exp (exp1)
                 (value-of/k exp1 env
                             (zero1-cont cont)))
      (if-exp (exp1 exp2 exp3)
              (value-of/k exp1 env
                          (if-test-cont exp2 exp3 env cont)))
      (let-exp (var exp body)
               (value-of/k exp env 
                         (let-exp-cont var body env cont)))
      (letrec-exp (p-name b-vars body letrec-body)
                  (value-of/k
                   letrec-body 
                   (extend-env-rec p-name b-vars body env)
                   cont))
      (letcc-exp (var body)
                 (value-of/k body (extend-env var cont env) cont))
      (throw-exp (exp1 exp2)
                 (value-of/k exp1 env (throw-cont exp2 env cont)))
      (try-exp (exp1 var exp2)
               (value-of/k exp1 env (try-cont var exp2 env cont)))
      (raise-exp (exp1)
                 (value-of/k exp1 env (raise-cont cont)))
      (proc-exp (vars body)
                (apply-cont cont (proc-val (procedure vars body env))))
      (call-exp (rator rand)
                (value-of/k rator env
                            (rator-cont rand env cont))))))
(define apply-procedure/k
  (lambda (proc1 val cont)
    (cases proc proc1
      (procedure (var body saved-env)
                 (value-of/k body 
                             (extend-env var val saved-env)
                             cont)))))


;(run "try let x = 1
;            in  if    zero?(x)
;                then  42
;                else  raise x
;       catch(exception) exception")
;(run "let x = 1
;        in -(2,letcc y let z = 1
;                       in  throw z to y)")

  
  
  
  
  
