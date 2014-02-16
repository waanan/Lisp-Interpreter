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

;;env
(define-datatype environment environment?
  (empty-env)
  (extend-env
   (var symbol?)
   (val integer?)
   (env environment?)))
(define init-env
  (lambda ()
    (empty-env)))
(define apply-env
  (lambda (env search-var)
    (cases environment env
      (empty-env ()
                 (report-no-binding-found search-var))
      (extend-env (saved-var saved-val saved-env)
                  (if (eq? saved-var search-var)
                      saved-val
                      (apply-env saved-env search-var))))))
(define report-no-binding-found
  (lambda (search-var)
    (eopl:error 'apply-env "No binding for ~s" search-var)))

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
(define-datatype proc proc?
  (procedure
   (vars (list-of symbol?))
   (body expression?)
   (saved-env environment?)))
(define report-expval-extractor-error 
  (lambda (x y)
    (eopl:error x "~s extract error" y)))


;;datatype
(define-datatype program program?
  (a-program
   (state statement?)))
(define-datatype statement statement?
  (assign-state
   (var symbol?)
   (exp expression?))
  (read-state
   (var symbol?))
  (print-state
   (exp1 expression?))
  (states-state
   (states (list-of statement?)))
  (if-state
   (exp expression?)
   (state1 statement?)
   (state2 statement?))
  (while-state
   (exp expression?)
   (state statement?))
  (var-state
   (vars (list-of symbol?))
   (state statement?))
  (sub-state
   (vars (list-of symbol?))
   (states (list-of statement?))
   (state statement?))
  (call-state
   (var symbol?)))
(define-datatype expression expression?
  (const1-exp
   (num number?))
  (const2-exp
   (bool symbol?))
  (if-exp
   (exp1 expression?)
   (exp2 expression?)
   (exp3 expression?))
  (diff-exp
   (exp1 expression?)
   (exp2 expression?))
  (zero?-exp
   (exp1 expression?))
  (var-exp
   (var symbol?))
  (add-exp
   (exp1 expression?)
   (exp2 expression?))
  (multi-exp
   (exp1 expression?)
   (exp2 expression?))
  (not-exp
   (exp expression?))
  (proc-exp 
   (vars (list-of symbol?))
   (body expression?))
  (call-exp
   (rator expression?)
   (rands (list-of expression?))))

;;help-function
(define list-of
  (lambda (pred)
    (lambda (lst)
      (if (null? lst)
          #t
          (and (pair? lst)
               (pred (car lst))
               ((list-of pred) (cdr lst)))))))

;;scan-parse
(define scanner-spec
  '((white-sp (whitespace) skip)
    (identifier (letter (arbno (or letter digit))) symbol)
    ;    (boolean ("#" (or "t" "f")) symbol) 
    (number ((or "" "-") digit (arbno digit)) number)))
(define grammar
  '((program
     (statement)
     a-program)
    (statement
     (identifier "=" expression)
     assign-state)
    (statement
     ("read" "(" identifier ")")
     read-state)
    (statement
     ("print" expression)
     print-state)
    (statement
     ("{" (separated-list statement ";") "}")
     states-state)
    (statement
     ("if" expression statement statement)
     if-state)
    (statement
     ("while" expression statement)
     while-state)
    (statement
     ("var" (separated-list identifier ",") ";" statement)
     var-state)
    (statement
     ("sub" (arbno identifier "=" statement) "in" statement)
    sub-state)
    (statement
     ("call-state" "(" identifier ")")
     call-state)
    
    
    (expression
     (number)
     const1-exp)
    (expression
     ("#" identifier)
     const2-exp)
    (expression
     (identifier)
     var-exp)
    (expression
     ("if" expression "then" expression "else" expression)
     if-exp)
    (expression
     ("zero?" "(" expression ")")
     zero?-exp)
    (expression
     ("-" "(" expression "," expression ")")
     diff-exp)
    (expression
     ("+" "(" expression "," expression ")")
     add-exp)
    (expression
     ("*" "(" expression "," expression ")")
     multi-exp)
    (expression
     ("not" "(" expression ")")
     not-exp)
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

(define run
  (lambda (string)
    (run-program (scan&parse string))))
(define run-program
  (lambda (pgm)
    (cases program pgm
      (a-program (state)
                 (run-state state (init-env))))))
(define run-state
  (lambda (state env)
    (cases statement state
       (assign-state (var exp) 
                     (setref! (apply-env env var) (value-of exp env)))
      (read-state (var)
                  (let ((val (read)))
                   (if (integer? val)
                       (setref! (apply-env env var) (num-val val))
                       (eopl:error 'read-state "input not integer ~s" val))))
      (print-state (exp)
                   (let ((val (value-of exp env)))
                     (cases expval val
                       (num-val
                        (num) (eopl:printf "~s~%" num))
                       (bool-val
                        (bool) (eopl:printf "~s~%" bool))
                       (proc-val
                        (proc) (eopl:printf "~s~%" proc)))))
      (states-state (states)
                    (run-states states env))
      (if-state (exp state1 state2)
                (let ((val (expval->bool (value-of exp env))))
                  (if (val)
                      (run-state state1 env)
                      (run-state state2 env))))
      (while-state (exp state)
                  (let ((val (expval->bool (value-of exp env))))
                    (cond (val
                            (begin
                              (run-state state env)
                              (run-state (while-state exp state) env))))))
      (var-state (vars state)
                 (let ((new-env (extend-envs env vars)))
                   (run-state state new-env)))
      (sub-state (vars states state)
                 (run-state state (extend-env-states env vars states)))
      (call-state (var)
                  (let ((state (deref (apply-env env var))))
                    (run-state state env))))))
(define extend-envs
  (lambda (env vars)
    (if (null? vars)
        env
        (extend-envs (extend-env (car vars) (newref (num-val 0)) env) (cdr vars)))))
(define extend-env-states
  (lambda (env vars states)
    (if (null? vars)
        env
        (extend-env-states (extend-env (car vars) (newref (car states)) env) (cdr vars) (cdr states)))))
  
  
;;state-help
(define run-states
  (lambda (states env)
    (cond ((not (null? states))
           (begin 
             (run-state (car states) env)
             (run-states (cdr states) env))))))

(define value-of
  (lambda (exp env)
    (cases expression exp
      (const1-exp (num) (num-val num))
      (const2-exp (bool) (if (eq? bool 't)
                             (bool-val #t)
                             (bool-val #f)))
      (var-exp (var) (deref (apply-env env var)))
      (if-exp (exp1 exp2 exp3)
              (let ((val (expval->bool (value-of exp1 env))))
                (if val
                    (value-of exp2 env)
                    (value-of exp3 env))))
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
      (not-exp (exp)
               (let ((val (value-of exp env)))
                 (if (expval->bool val)
                     (bool-val #f) 
                     (bool-val #t))))
      (proc-exp (vars body)
                (proc-val (procedure vars body env)))
      (call-exp (rator rands)
                (let ((proc (expval->proc (value-of rator env))))
                  (apply-procedure proc rands env))))))
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
             (apply-help rest-vars rest-rands body (extend-env var (newref arg) saved-env) now-env)))))) 






















