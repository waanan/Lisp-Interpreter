#lang eopl
(define exp 'uninitialized)
(define env 'uninitialized)
(define cont 'uninitialized)
(define val 'uninitialized)
(define proc1 'uninitialized)

(define-datatype environment environment?
  (empty-env)
  (extend-env
   (var symbol?)
   (val expval?)
   (env environment?))
  (extend-env-rec
   (p-name symbol?)
   (b-vars symbol?)
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
   (b-vars symbol?)
   (body expression?)
   (letrec-body expression?))
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
  (rator-cont
   (rand expression?)
   (env environment?)
   (cont continuation?))
  (rand-cont
   (val1 expval?)
   (cont continuation?)))
(define apply-cont
  (lambda ()
    (cases continuation cont
      (end-cont ()
                (begin 
                  (eopl:printf "End of computation.~%")
                  val))
      (zero1-cont (saved-cont)
                  (set! cont saved-cont)
                  (set! val (bool-val (= (expval->num val) 0)))
                  (apply-cont))  
      (let-exp-cont (var body saved-env saved-cont)
                    (set! cont saved-cont)
                    (set! env (extend-env var val saved-env))
                    (set! exp body)
                    (value-of/k)) 
      (if-test-cont (exp2 exp3 saved-env saved-cont)
                    (set! cont saved-cont)
                    (set! env saved-env)              
                    (if (expval->bool val)
                        (set! exp exp2)
                        (set! exp exp3))
                        (value-of/k))
      (diff1-cont (exp2 saved-env saved-cont)
                  (set! exp exp2)
                  (set! env saved-env)
                  (set! cont (diff2-cont val saved-cont))
                  (value-of/k))
      (diff2-cont (val1 saved-cont)
                  (let ((num1 (expval->num val1))
                        (num2 (expval->num val)))
                    (set! cont saved-cont)
                    (set! val (num-val (- num1 num2)))
                    (apply-cont)))
      (rator-cont (rand saved-env saved-cont)
                  (set! env saved-env)
                  (set! cont (rand-cont val saved-cont))
                  (set! exp rand)
                  (value-of/k))                      
      (rand-cont (val1 saved-cont)
                 (let ((proc (expval->proc val1)))
                   (set! proc1 proc)
                   (set! cont saved-cont)
                   (apply-procedure/k))))))

    
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
     ("letrec" identifier "(" identifier ")" "=" expression "in" expression)
     letrec-exp)
    (expression
     ("proc" "(" identifier ")" expression)
     proc-exp)
    (expression
     ("(" expression expression")")
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
   (vars symbol?)
   (body expression?)
   (saved-env environment?)))

(define run
  (lambda (string)
    (value-of-program (scan&parse string))))
(define value-of-program
  (lambda (pgm)
    (cases program pgm
      (a-program (exp1)
                 (set! cont (end-cont))
                 (set! env (init-env))
                 (set! exp exp1)
                 (value-of/k)))))
(define value-of/k
  (lambda ()
    (cases expression exp
      (const1-exp (num) 
                  (set! val (num-val num))
                  (apply-cont))
      (const2-exp (bool) 
                  (set! val (if (eq? bool 't)
                                (bool-val #t)
                                (bool-val #f)))
                  (apply-cont))
      (var-exp (var) 
               (set! val (apply-env env var))
               (apply-cont))
      (diff-exp (exp1 exp2)
                (set! cont (diff1-cont exp2 env cont))
                (set! exp exp1)
                (value-of/k))
      (zero?-exp (exp1)
                 (set! cont (zero1-cont cont))
                 (set! exp exp1)
                 (value-of/k))
      (if-exp (exp1 exp2 exp3)
              (set! cont (if-test-cont exp2 exp3 env cont))
              (set! exp exp1)
              (value-of/k))                          
      (let-exp (var exp1 body)
               (set! cont (let-exp-cont var body env cont))
               (set! exp exp1)
               (value-of/k))
      (letrec-exp (p-name b-vars body letrec-body)
                  (set! exp letrec-body)
                  (set! env (extend-env-rec p-name b-vars body env))
                  (value-of/k))
      (proc-exp (vars body)
                (set! val (proc-val (procedure vars body env)))
                (apply-cont))
      (call-exp (rator rand)
                (set! cont (rator-cont rand env cont))
                (set! exp rator)
                (value-of/k)))))
(define apply-procedure/k
  (lambda ()
    (cases proc proc1
      (procedure (var body saved-env)
                 (set! env (extend-env var val saved-env))
                 (set! exp body)
                 (value-of/k)))))






  
  
  
  
  
