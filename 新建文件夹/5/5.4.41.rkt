#lang eopl

(define cont 'uninitialized)
(define temp-val 'uninitialized)

(define-datatype environment environment?
  (empty-env)
  (extend-env
   (var symbol?)
   (val expval?)
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



(define end-cont
  (lambda ()
    (lambda (x)
      (if (equal? x 'normal)  
          (lambda (val)
            (begin 
              (eopl:printf "End of computation.~%")
              val))
          (eopl:error "Uncaught exception!")))))
          
(define zero1-cont
  (lambda (saved-cont)
    (lambda (x)
      (if (equal? x 'normal)  
          (lambda (val)
            (set! cont saved-cont)
            (apply-cont (bool-val (= (expval->num val) 0))))
          (begin (set! cont saved-cont)
                 (apply-handler))))))

(define let-exp-cont
  (lambda (var body saved-env saved-cont)
    (lambda (x)
      (if (equal? x 'normal)  
          (lambda (val)
            (set! cont saved-cont)
            (value-of/k body
                        (extend-env var val saved-env)))
          (begin (set! cont saved-cont)
                 (apply-handler))))))

(define if-test-cont
  (lambda (exp2 exp3 saved-env saved-cont)
    (lambda (x)
      (if (equal? x 'normal)  
          (lambda (val)
            (set! cont saved-cont)
            (if (expval->bool val)
                (value-of/k exp2 saved-env)
                (value-of/k exp3 saved-env)))
          (begin (set! cont saved-cont)
                 (apply-handler))))))
(define diff1-cont
  (lambda (exp2 env saved-cont)
    (lambda (x)
      (if (equal? x 'normal)  
          (lambda (val)
            (set! cont (diff2-cont val saved-cont))
            (value-of/k exp2 env))
          (begin (set! cont saved-cont)
                 (apply-handler))))))
(define diff2-cont
  (lambda (val1 saved-cont)
    (lambda (x)
      (if (equal? x 'normal)  
          (lambda (val)
            (set! cont saved-cont)
            (let ((num1 (expval->num val1))
                  (num2 (expval->num val)))
              (apply-cont (num-val (- num1 num2)))))
          (begin (set! cont saved-cont)
                 (apply-handler))))))
(define try-cont
  (lambda (var exp env saved-cont)
    (lambda (x)
      (if (equal? x 'normal)  
          (lambda (val)
            (set! cont saved-cont)
            (apply-cont val))
          (begin (set! cont saved-cont)
                 (value-of/k exp (extend-env var temp-val env)))))))
(define raise-cont
  (lambda (saved-cont)
    (lambda (x)
      (if (equal? x 'normal)
          (lambda (val)
            (set! cont saved-cont)
            (set! temp-val val)
            (apply-handler))
          (begin (set! cont saved-cont)
                 (apply-handler))))))
(define rator-cont
  (lambda (rand env saved-cont)
    (lambda (x)
      (if (equal? x 'normal)
          (lambda (val)
            (set! cont (rand-cont val saved-cont))
            (value-of/k rand env))
          (begin (set! cont saved-cont)
                 (apply-handler))))))
(define rand-cont
  (lambda (val1 saved-cont)
    (lambda (x)
      (if (equal? x 'normal)
          (lambda (val)
            (set! cont saved-cont)
            (let ((proc1 (expval->proc val1)))
              (apply-procedure/k proc1 val)))
          (begin (set! cont saved-cont)
                 (apply-handler))))))
(define apply-cont
  (lambda (val)
    ((cont 'normal) val)))
(define apply-handler
  (lambda ()
    (cont 'raise)))

    
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
                 (set! cont (end-cont))
                 (value-of/k exp1 (init-env))))))
(define value-of/k
  (lambda (exp env)
    (cases expression exp
      (const1-exp (num) (apply-cont (num-val num)))
      (const2-exp (bool) (apply-cont    (if (eq? bool 't)
                                            (bool-val #t)
                                            (bool-val #f))))
      (var-exp (var) (apply-cont (apply-env env var)))
      (diff-exp (exp1 exp2)
                (set! cont (diff1-cont exp2 env cont))
                (value-of/k exp1 env))
      (zero?-exp (exp1)
                 (set! cont (zero1-cont cont))
                 (value-of/k exp1 env))
      (if-exp (exp1 exp2 exp3)
              (set! cont (if-test-cont exp2 exp3 env cont))
              (value-of/k exp1 env))
      (let-exp (var exp body)
               (set! cont (let-exp-cont var body env cont)) 
               (value-of/k exp env))
      (letrec-exp (p-name b-vars body letrec-body)
                  (value-of/k letrec-body 
                              (extend-env-rec p-name b-vars body env)))
      (try-exp (exp1 var exp2)
               (set! cont (try-cont var exp2 env cont))
               (value-of/k exp1 env))
      (raise-exp (exp1)
                 (set! cont (raise-cont cont))
                 (value-of/k exp1 env))
      (proc-exp (vars body)
                (apply-cont (proc-val (procedure vars body env))))
      (call-exp (rator rand)
                (set! cont (rator-cont rand env cont))
                (value-of/k rator env)))))
(define apply-procedure/k
  (lambda (proc1 val)
    (cases proc proc1
      (procedure (var body saved-env)
                 (value-of/k body 
                             (extend-env var val saved-env))))))


;(run "try let x = 1
;            in  if    zero?(x)
;                then  42
;                else  raise x
;       catch(exception) exception")


  
  
  
  
  
