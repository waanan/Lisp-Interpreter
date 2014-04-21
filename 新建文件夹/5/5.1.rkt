#lang eopl

;;store
(define the-store '())
(define empty-store
  (lambda () '()))
(define get-store
  (lambda () the-store))
(define initialize-store!
  (lambda ()
    (set! the-store (empty-store))))
(define reference?
  (lambda (v)
    (integer? v)))
(define newref
  (lambda (val)
    (let ((next-ref (length the-store)))
      (set! the-store (append the-store (list val)))
      next-ref)))
(define deref
  (lambda (ref)
    (list-ref the-store ref)))
(define setref!
  (lambda (ref val)
    (set! the-store
          (letrec
              ((setref-inner
                (lambda (store1 ref1)
                  (cond ((null? store1)
                         (eopl:error 'setref "Invalid reference ~s ~s" ref val))
                        ((= ref1 0)
                         (cons val (cdr store1)))
                        (else
                         (cons (car store1) 
                               (setref-inner
                                (cdr store1) (- ref1 1))))))))
            (setref-inner the-store ref)))))
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

;;env
(define-datatype environment environment?
  (empty-env)
  (extend-env
   (var symbol?)
   (pos integer?)
   (env environment?))
  (extend-env-rec
   (p-name symbol?)
   (b-vars (list-of symbol?))
   (body expression?)
   (env environment?)))

(define init-env
  (lambda ()
    (extend-env
     'i (newref (num-val 5))
     (extend-env
      'v (newref (num-val 2))
      (extend-env
       'x (newref (num-val 10))
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
  (null-exp)
  (null?-exp
   (exp1 expression?))
  (cons-exp
   (exp1 expression?)
   (exp2 expression?))
  (car-exp
   (exp1 expression?))
  (cdr-exp 
   (exp1 expression?))
  (list-exp
   (exps (list-of expression?)))
  (diff-exp
   (exp1 expression?)
   (exp2 expression?))
  (multi-exp
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
   (vars (list-of symbol?))
   (exps (list-of expression?))
   (body expression?))
  (letrec-exp 
   (p-name symbol?)
   (b-vars (list-of symbol?))
   (body expression?)
   (letrec-body expression?))
  (set-exp
   (var symbol?)
   (exp1 expression?))
  (beg-exp
   (exps (list-of expression?)))
  (proc-exp 
   (vars (list-of symbol?))
   (exp1 expression?))
  (call-exp
   (rator expression?)
   (rands (list-of expression?))))

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
     ("emptylist")
     null-exp)
    (expression
     ("null?" "(" expression ")")
     null?-exp)
    (expression
     ("cons" "(" expression "," expression ")")
     cons-exp)
    (expression
     ("car" "(" expression")")
     car-exp)
    (expression
     ("cdr" "(" expression")")
     cdr-exp)
    (expression
     ("list" "(" (separated-list expression ",") ")")
     list-exp)
    (expression
     ("-" "(" expression "," expression ")")
     diff-exp)
    (expression
     ("*" "(" expression "," expression ")")
     multi-exp)
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
     ("let" (arbno identifier "=" expression) "in" expression)
     let-exp)  
    (expression
     ("letrec" identifier "(" (separated-list identifier ",") ")" "=" expression "in" expression)
     letrec-exp)
    (expression
     ("set" identifier "=" expression)
     set-exp)
    (expression
     ("begin" (separated-list expression ";") "end")
     beg-exp)
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

(define-datatype continuation continuation?
  (end-cont)
  
  (null?-cont
   (cont continuation?))
  (cons1-cont
   (exp1 expression?)
   (env environment?)
   (cont continuation?))
  (cons2-cont
   (val1 expval?)
   (cont continuation?))
  (car-cont
   (cont continuation?))
  (cdr-cont
   (cont continuation?))
  (list-cont
   (epxvals (list-of expval?))
   (exps (list-of expression?))
   (env environment?)
   (cont continuation?))
  (zero1-cont
   (cont continuation?))
  (let-exp-cont
   (var symbol?)
   (vars (list-of symbol?))
   (exps (list-of expression?))
   (body expression?)
   (const-env environment?)
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
  (multi1-cont
   (exp2 expression?)
   (env environment?)
   (cont continuation?))
  (multi2-cont
   (val1 expval?)
   (cont continuation?))
  (set-cont
   (var symbol?)
   (env environment?)
   (cont continuation?))
  (beg-cont
   (exps (list-of expression?))
   (env environment?)
   (cont continuation?))
  (rator-cont
   (rands (list-of expression?))
   (env environment?)
   (cont continuation?))
  (rands-cont
   (val1 expval?)
   (rands (list-of expval?))
   (rest-rands (list-of expression?))
   (env environment?)
   (cont continuation?)))
(define apply-cont
  (lambda (cont val)
    (cases continuation cont
      (end-cont ()
                (begin 
                  (eopl:printf "End of computation.~%")
                  val))
      (null?-cont (saved-cont)
                  (let ((lst (expval->ls val)))
                    (cases ls lst
                      (null-val ()
                                (apply-cont saved-cont (bool-val #t)))
                      (cons-val (val1 val2)
                                (apply-cont saved-cont (bool-val #f))))))
      (cons1-cont (exp2 saved-env saved-cont)
                  (value-of/k exp2 saved-env (cons2-cont val saved-cont)))
      (cons2-cont (val1 saved-cont)
                  (apply-cont saved-cont (ls-val (cons-val val1 val))))
      (car-cont (saved-cont)
                (let ((lst (expval->ls val)))
                  (cases ls lst
                    (null-val ()
                              (eopl:error "list is null ~s" 'car-exp)) 
                    (cons-val (val1 val2)
                              (apply-cont saved-cont val1)))))
      (cdr-cont (saved-cont)
                (let ((lst (expval->ls val)))
                  (cases ls lst
                    (null-val ()
                              (eopl:error "list is null ~s" 'cdr-exp)) 
                    (cons-val (val1 val2)
                              (apply-cont saved-cont val2))))) 
      (list-cont (expvals exps env saved-cont)
                 (if (null? exps)
                     (apply-cont saved-cont (list-help expvals))
                     (value-of/k (car exps) env (list-cont (cons val expvals) (cdr exps) env saved-cont))))
                     
      
      (zero1-cont (saved-cont)
                  (apply-cont saved-cont 
                              (bool-val (= (expval->num val) 0))))
      (let-exp-cont (var vars exps body const-env saved-env saved-cont)
                    (if (null? vars)
                        (value-of/k body
                                (extend-env var (newref val) saved-env)
                                saved-cont)
                        (value-of/k (car exps)
                                    const-env
                                    (let-exp-cont (car vars) (cdr vars) (cdr exps) body 
                                                  const-env
                                                  (extend-env var (newref val) saved-env)
                                                  saved-cont))))
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
      (multi1-cont (exp2 env cont)
                  (value-of/k exp2 env (multi2-cont val cont)))
      (multi2-cont (val1 cont)
                  (let ((num1 (expval->num val1))
                        (num2 (expval->num val)))
                    ;(eopl:printf "~s * ~s~%" num1 num2)
                    (apply-cont cont
                                (num-val (* num1 num2)))))
      (set-cont (var env saved-cont)
                (let ((n (apply-env env var)))
                  (setref! n val)
                  (apply-cont saved-cont (num-val 42))))
      (beg-cont (exps env cont)
                (if (null? exps)
                    (apply-cont cont val)
                    (value-of/k (car exps) env (beg-cont (cdr exps) env cont))))
                    
      (rator-cont (rands env cont)
                  (if (null? rands)
                      (let ((proc1 (expval->proc val)))
                        (apply-procedure/k proc1 '() cont))
                      (value-of/k (car rands) env
                                  (rands-cont val '() (cdr rands) env cont))))
      (rands-cont (val1 rands rest-rands env cont)
                 (if (null? rest-rands)
                     (let ((proc1 (expval->proc val1)))
                        (apply-procedure/k proc1 (append rands (list val)) cont))
                     (value-of/k (car rest-rands) env
                                  (rands-cont val1 (append rands (list val)) (cdr rest-rands) env cont)))))))
;;help-function
(define list-iter
  (lambda (expvals result)
    (if (null? expvals)
        result
        (list-iter (cdr expvals) (ls-val (cons-val (car expvals) result)))))) 
(define list-help
  (lambda (expvals)
    (list-iter expvals (ls-val (null-val)))))
    
    




;;基本数据类型
(define-datatype expval expval?
  (num-val
   (num number?))
  (bool-val
   (bool boolean?))
  (proc-val
   (proc proc?))
  (ls-val
   (lst ls?)))
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
(define expval->ls
   (lambda (val)
    (cases expval val
      (ls-val (lst) lst)
      (else (report-expval-extractor-error 'lst val)))))

(define report-expval-extractor-error 
  (lambda (x y)
    (eopl:error x "~s extract error" y)))
  
(define-datatype proc proc?
  (procedure
   (vars (list-of symbol?))
   (body expression?)
   (saved-env environment?)))

(define-datatype ls ls?
  (null-val)
  (cons-val
   (car expval?)
   (cdr expval?)))



(define run
  (lambda (string)
    (value-of-program (scan&parse string))))
(define value-of-program
  (lambda (pgm)
    (initialize-store!)
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
      
      (null-exp () (apply-cont cont (ls-val (null-val))))
      (null?-exp (exp1)
                 (value-of/k exp1 env (null?-cont cont)))
      (cons-exp (exp1 exp2)
                (value-of/k exp1 env (cons1-cont exp2 env cont)))
      (car-exp (exp1)
               (value-of/k exp1 env (car-cont cont)))
      (cdr-exp (exp1)
               (value-of/k exp1 env (cdr-cont cont))) 
      
      (list-exp (exps)
                (if (null? exps)
                    (apply-cont cont (ls-val (null-val)))
                    (value-of/k (car exps) env (list-cont '() (cdr exps) env cont))))
      (var-exp (var) (apply-cont cont 
                                 (let ((n (apply-env env var)))
                                  (if (integer? n)
                                      (deref n)
                                      n))))
      (diff-exp (exp1 exp2)
                (value-of/k exp1 env 
                          (diff1-cont exp2 env cont)))
      (multi-exp (exp1 exp2)
                (value-of/k exp1 env 
                            (multi1-cont exp2 env cont)))
      (zero?-exp (exp1)
                 (value-of/k exp1 env
                             (zero1-cont cont)))
      (if-exp (exp1 exp2 exp3)
              (value-of/k exp1 env
                          (if-test-cont exp2 exp3 env cont)))
      (let-exp (vars exps body)
               (if (null? vars)
                   (apply-cont cont body)
                   (value-of/k (car exps) env 
                         (let-exp-cont (car vars) vars exps body env env cont))))
      (letrec-exp (p-name b-vars body letrec-body)
                  (value-of/k
                   letrec-body 
                   (extend-env-rec p-name b-vars body env)
                   cont))
      (set-exp (var exp1) 
               (value-of/k exp1 env (set-cont var env cont)))
      (beg-exp (exps)
               (value-of/k (car exps) env (beg-cont (cdr exps) env cont)))
      (proc-exp (vars body)
                (apply-cont cont (proc-val (procedure vars body env))))
      (call-exp (rator rands)
                (value-of/k rator env
                            (rator-cont rands env cont))))))
(define apply-procedure/k
  (lambda (proc1 vals cont)
    (cases proc proc1
      (procedure (vars body saved-env)
                 (value-of/k body 
                             (apply-help vars vals saved-env)
                             cont)))))
(define apply-help
  (lambda (vars vals saved-env)
           (if (null? vars)
               saved-env
               (apply-help (cdr vars) (cdr vals)
                           (extend-env (car vars) (newref (car vals)) saved-env)))))

;(run "letrec fact(n) = if    zero?(-(n,1))
;                         then  1
;                         else  *(n,(fact -(n,1)))
;               in (fact 4)")

;(run "letrec factiter(n,x) = if    zero?(-(n,1))
;                                then  x
;                                else  (factiter -(n,1) *(n,x))
;               in (factiter 4 1)")

  
  
  
  
  
