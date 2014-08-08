#lang eopl
;;registerize
(define val '())
(define env '())
(define cont '())
(define exp '())
(define th '())

;;queue

(define the-ready-queue '())
(define the-final-answer '())
(define the-max-time-slice '())
(define the-time-remaining '())

(define empty-queue
  (lambda () '()))
(define empty?
  (lambda (queue)
    (null? queue)))
(define enqueue
  (lambda (queue th)
    (append queue (list th))))
(define dequeue 
  (lambda (queue op)
    (op (car queue) (cdr queue))))


(define initialize-scheduler!
  (lambda (ticks)
    (set! the-ready-queue (empty-queue))
    (set! the-final-answer 'unintialized)
    (set! the-max-time-slice ticks)
    (set! the-time-remaining the-max-time-slice)))
(define place-on-ready-queue!
  (lambda ()
    (set! the-ready-queue
          (enqueue the-ready-queue th))))
(define run-next-thread
  (lambda ()
    (if (empty? the-ready-queue)
        (begin 
          (eopl:printf "End of computation.~%")
          the-final-answer)
        (dequeue the-ready-queue
                 (lambda (first-ready-thread other-ready-threads)
                   (set! the-ready-queue other-ready-threads)
                   (set! the-time-remaining the-max-time-slice)
                   (set! cont (car first-ready-thread))
                   (set! val (cdr first-ready-thread))
                   (apply-cont))))))
(define set-final-answer!
  (lambda (val)
    (set! the-final-answer val)))
(define time-expired?
  (lambda ()
    (zero? the-time-remaining)))
(define decrement-timer!
  (lambda ()
    (set! the-time-remaining (- the-time-remaining 1))))




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

;mutex
(define-datatype mutex mutex?
  (a-mutex
   (ref-to-closed? reference?)
   (ref-to-wait-queue reference?)))
(define new-mutex
  (lambda ()
    (a-mutex
     (newref #f)
     (newref '()))))

(define-datatype program program?
  (a-program
   (exp1 expression?)))
(define-datatype expression expression?
  (const1-exp
   (num number?))
  (const2-exp
   (bool symbol?))
  (null-exp)
  (mutex-exp)
  (wait-exp
   (exp1 expression?))
  (signal-exp
   (exp1 expression?))
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
  (print-exp
   (exp1 expression?))
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
  (spwan-exp 
   (exp1 expression?))
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
     ("mutex" "(" ")")
     mutex-exp)
    (expression
     ("wait" "(" expression ")")
     wait-exp)
    (expression
     ("signal" "(" expression ")")
     signal-exp)
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
     ("print" "(" expression ")")
     print-exp)
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
     ("spawn" "(" expression ")")
     spwan-exp)
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
  (end-main-thread-cont)
  (end-sub-thread-cont)
  (null?-cont
   (cont continuation?))
  (cons1-cont
   (exp1 expression?)
   (env environment?)
   (cont continuation?))
  (cons2-cont
   (val1 expval?)
   (cont continuation?))
  (th-cont
   (val1 expval?)
   (cont continuation?))
  (wait-cont
   (cont continuation?))
  (signal-cont
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
  (print-cont
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
  (spwan-cont
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
  (lambda ()
    (if (time-expired?)
        (begin
          (set! th (cons cont val))
          (place-on-ready-queue!)
          (run-next-thread))
        (begin 
          (decrement-timer!)
          (cases continuation cont
            (end-main-thread-cont ()
                                  (set-final-answer! val)
                                  (run-next-thread))
            (end-sub-thread-cont ()
                                 (run-next-thread))
            (null?-cont (saved-cont)
                        (let ((lst (expval->ls val)))
                          (cases ls lst
                            (null-val ()
                                      (set! val (bool-val #t))
                                      (set! cont saved-cont)
                                      (apply-cont))
                            (cons-val (val1 val2)
                                      (set! val (bool-val #f))
                                      (set! cont saved-cont)
                                      (apply-cont)))))
            (cons1-cont (exp2 saved-env saved-cont)
                        (set! exp exp2)
                        (set! env saved-env)
                        (set! cont (cons2-cont val saved-cont))
                        (value-of/k))
            (cons2-cont (val1 saved-cont)
                        (set! val (ls-val (cons-val val1 val)))
                        (set! cont saved-cont)
                        (apply-cont))
            (wait-cont (saved-cont)
                       (set! val (expval->mutex val))
                       (set! th (cons saved-cont (num-val 52)))
                       (wait-for-mutex))
            (signal-cont (saved-cont)
                         (set! val (expval->mutex val))
                         (set! exp (cons saved-cont (num-val 53)))
                         (signal-mutex))
            (car-cont (saved-cont)
                      (let ((lst (expval->ls val)))
                        (cases ls lst
                          (null-val ()
                                    (eopl:error "list is null ~s" 'car-exp)) 
                          (cons-val (val1 val2)
                                    (set! val val1)
                                    (set! cont saved-cont)
                                    (apply-cont)))))
            (cdr-cont (saved-cont)
                      (let ((lst (expval->ls val)))
                        (cases ls lst
                          (null-val ()
                                    (eopl:error "list is null ~s" 'cdr-exp)) 
                          (cons-val (val1 val2)
                                    (set! val val2)
                                    (set! cont saved-cont)
                                    (apply-cont))))) 
            (list-cont (expvals exps saved-env saved-cont)
                       (if (null? exps)
                           (begin 
                             (set! env saved-env)
                             (set! cont saved-cont)
                             (set! val (list-help (cons val expvals)))
                             (apply-cont))
                           (begin
                             (set! env saved-env)
                             (set! exp (car exps))
                             (set! cont (list-cont (cons val expvals) (cdr exps) env saved-cont))
                             (value-of/k))))
            
            (print-cont (saved-cont)
                        (display val)
                        (newline)
                        (set! val (num-val 10))
                        (set! cont saved-cont)
                        (apply-cont))
            (zero1-cont (saved-cont)
                        (set! val (bool-val (= (expval->num val) 0)))
                        (set! cont saved-cont)
                        (apply-cont))
            (let-exp-cont (var vars exps body const-env saved-env saved-cont)
                          (if (null? vars)
                              (begin
                                (set! env (extend-env var (newref val) saved-env))
                                (set! exp body)
                                (set! cont saved-cont)
                                (value-of/k))
                              (begin
                                (set! exp (car exps))
                                (set! env const-env)
                                (set! cont (let-exp-cont (car vars) (cdr vars) (cdr exps) body 
                                                        const-env
                                                        (extend-env var (newref val) saved-env)
                                                        saved-cont))
                                (value-of/k))))
            (if-test-cont (exp2 exp3 saved-env saved-cont)
                          (if (expval->bool val)
                              (begin 
                                (set! exp exp2)
                                (set! env saved-env)
                                (set! cont saved-cont)
                                (value-of/k))
                               (begin 
                                (set! exp exp3)
                                (set! env saved-env)
                                (set! cont saved-cont)
                                (value-of/k))))
            (diff1-cont (exp2 saved-env saved-cont)
                        (set! exp exp2)
                        (set! env saved-env)
                        (set! cont (diff2-cont val saved-cont))
                        (value-of/k))
            (diff2-cont (val1 saved-cont)
                        (let ((num1 (expval->num val1))
                              (num2 (expval->num val)))
                          (set! val (num-val (- num1 num2)))
                          (set! cont saved-cont)
                          (apply-cont)))
            (multi1-cont (exp2 saved-env saved-cont)
                         (set! exp exp2)
                         (set! env saved-env)
                         (set! cont (multi2-cont val saved-cont))
                         (value-of/k))
            (multi2-cont (val1 saved-cont)
                         (let ((num1 (expval->num val1))
                               (num2 (expval->num val)))
                           ;(eopl:printf "~s * ~s~%" num1 num2)
                           (set! val (num-val (* num1 num2)))
                           (set! cont saved-cont)
                           (apply-cont)))
            (set-cont (var saved-env saved-cont)
                      (let ((n (apply-env env var)))
                        (setref! n val)
                        (set! val (num-val 42))
                        (set! env saved-env)
                        (set! cont saved-cont)
                        (apply-cont)))
            (beg-cont (exps saved-env saved-cont)
                      (if (null? exps)
                          (begin
                            (set! env saved-env)
                            (set! cont saved-cont)
                            (apply-cont))
                          (begin
                            (set! env saved-env)
                            (set! exp (car exps))
                            (set! cont (beg-cont (cdr exps) env saved-cont))
                            (value-of/k))))
            
            (spwan-cont (saved-cont)
                          (set! th (cons (th-cont val (end-sub-thread-cont)) (num-val 28)))
                          (place-on-ready-queue!)
                          (set! val (num-val 73))
                          (set! cont saved-cont)
                          (apply-cont))
            (th-cont (val1 saved-cont)
                     (let ((proc1 (expval->proc val1)))
                       (set! exp proc1)
                       (set! th (list val))
                       (set! cont saved-cont)
                       (apply-procedure/k)))
        
            (rator-cont (rands saved-env saved-cont)
                        (if (null? rands)
                            (let ((proc1 (expval->proc val)))
                              (set! exp proc1)
                              (set! th '())
                              (set! cont saved-cont)
                              (apply-procedure/k))
                            (begin 
                              (set! exp (car rands))
                              (set! env saved-env)
                              (set! cont (rands-cont val '() (cdr rands) env saved-cont))
                              (value-of/k))))
            (rands-cont (val1 rands rest-rands saved-env saved-cont)
                        (if (null? rest-rands)
                            (let ((proc1 (expval->proc val1)))
                              (set! exp proc1)
                              (set! th (append rands (list val)))
                              (set! cont saved-cont)
                              (apply-procedure/k))
                            (begin 
                              (set! exp (car rest-rands))
                              (set! env saved-env)
                              (set! cont (rands-cont val1 (append rands (list val)) (cdr rest-rands) env saved-cont))
                              (value-of/k)))))))))
;;help-function
(define list-iter
  (lambda (expvals result)
    (if (null? expvals)
        result
        (list-iter (cdr expvals) (ls-val (cons-val (car expvals) result)))))) 
(define list-help
  (lambda (expvals)
    (list-iter expvals (ls-val (null-val)))))

(define wait-for-mutex
  (lambda ()
    (cases mutex val
      (a-mutex (ref-to-closed? ref-to-wait-queue)
               (cond 
                 ((deref ref-to-closed?)
                  (setref! ref-to-wait-queue
                           (enqueue (deref ref-to-wait-queue) th))
                  (run-next-thread))
                 (else
                  (setref! ref-to-closed? #t)
                  (set! cont (car th))
                  (set! val (cdr th))
                  (apply-cont)))))))
(define signal-mutex
  (lambda ()
    (cases mutex val
      (a-mutex (ref-to-closed? ref-to-wait-queue)
               (let ((closed? (deref ref-to-closed?))
                     (wait-queue (deref ref-to-wait-queue)))
                 (if closed?
                     (if (empty? wait-queue)
                         (setref! ref-to-closed? #f)
                         (dequeue wait-queue
                                  (lambda (first-waiting-th other-waiting-ths)
                                    (set! th first-waiting-th)
                                    (place-on-ready-queue!)
                                    (setref! ref-to-wait-queue
                                             other-waiting-ths))))
                     (begin (display "signal open mutex")
                            (newline)))
                 (set! cont (car exp))
                 (set! val (cdr exp))
                 (apply-cont))))))

;;基本数据类型
(define-datatype expval expval?
  (num-val
   (num number?))
  (bool-val
   (bool boolean?))
  (mutex-val
   (mutex1 mutex?))
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
(define expval->mutex
  (lambda (val)
    (cases expval val
      (mutex-val (mutex1) mutex1)
      (else (report-expval-extractor-error 'mutex val)))))
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
  (lambda (string timeslice)
    (value-of-program (scan&parse string) timeslice)))
(define value-of-program
  (lambda (pgm timeslice)
    (initialize-store!)
    (initialize-scheduler! timeslice)
    (cases program pgm
      (a-program (exp1)
                 (set! exp exp1)
                 (set! env (init-env))
                 (set! cont (end-main-thread-cont))
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
      (null-exp () 
                (set! val (ls-val (null-val)))
                (apply-cont))
      (null?-exp (exp1)
                 (set! exp exp1)
                 (set! cont (null?-cont cont))
                 (value-of/k))
      (cons-exp (exp1 exp2)
                (set! exp exp1)
                (set! cont (cons1-cont exp2 env cont))
                (value-of/k))
      (car-exp (exp1)
               (set! exp exp1)
               (set! cont (car-cont cont))
               (value-of/k))
      (cdr-exp (exp1)
               (set! exp exp1)
               (set! cont (cdr-cont cont))
               (value-of/k)) 
      
      (list-exp (exps)
                (if (null? exps)
                    (begin 
                      (set! val (ls-val (null-val)))
                      (apply-cont))
                    (begin
                      (set! exp (car exps))
                      (set! cont (list-cont '() (cdr exps) env cont))
                      (value-of/k))))
      (var-exp (var) 
               (set! val (let ((n (apply-env env var)))
                                   (if (integer? n)
                                       (deref n)
                                       n)))
               (apply-cont))
      (print-exp (exp1)
                 (set! exp exp1)
                 (set! cont (print-cont cont))
                 (value-of/k))
      (mutex-exp ()
                 (set! val (mutex-val (new-mutex)))
                 (apply-cont))
      (wait-exp (exp1)
                (set! exp exp1)
                (set! cont (wait-cont cont))
                (value-of/k))
      (signal-exp (exp1)
                  (set! exp exp1)
                  (set! cont (signal-cont cont))
                  (value-of/k))
      (diff-exp (exp1 exp2)
                (set! exp exp1)
                (set! cont (diff1-cont exp2 env cont))
                (value-of/k))
      (multi-exp (exp1 exp2)
                 (set! exp exp1)
                 (set! cont (multi1-cont exp2 env cont))
                 (value-of/k))
      (zero?-exp (exp1)
                 (set! exp exp1)
                 (set! cont (zero1-cont cont))
                 (value-of/k))
      (if-exp (exp1 exp2 exp3)
              (set! exp exp1)
              (set! cont (if-test-cont exp2 exp3 env cont))
              (value-of/k))
      (let-exp (vars exps body)
               (if (null? vars)
                   (begin 
                     (set! val body)
                     (apply-cont))
                   (begin
                     (set! exp (car exps))
                     (set! cont (let-exp-cont (car vars) vars exps body env env cont))
                     (value-of/k))))
      (letrec-exp (p-name b-vars body letrec-body)
                  (set! exp letrec-body)
                  (set! env (extend-env-rec p-name b-vars body env))
                  (value-of/k))
      (set-exp (var exp1) 
               (set! exp exp1)
               (set! cont (set-cont var env cont))
               (value-of/k))
      (beg-exp (exps)
               (set! exp (car exps))
               (set! cont (beg-cont (cdr exps) env cont))
               (value-of/k))
      (spwan-exp (exp1)
                 (set! exp exp1)
                 (set! cont (spwan-cont cont))
                 (value-of/k))
      (proc-exp (vars body)
                (set! val (proc-val (procedure vars body env)))
                (apply-cont))
      (call-exp (rator rands)
                (set! exp rator)
                (set! cont (rator-cont rands env cont))
                (value-of/k)))))
(define apply-procedure/k
  (lambda ()
    (cases proc exp
      (procedure (vars body saved-env)
                 (set! exp body)
                 (set! env (apply-help vars th saved-env))
                 (value-of/k)))))
(define apply-help
  (lambda (vars vals saved-env)
    (if (null? vars)
        saved-env
        (apply-help (cdr vars) (cdr vals)
                    (extend-env (car vars) (newref (car vals)) saved-env)))))

;(run "letrec fact(n) = if    zero?(-(n,1))
;                         then  1
;                         else  *(n,(fact -(n,1)))
;               in (fact 4)"
;      5)

;(run "letrec factiter(n,x) = if    zero?(-(n,1))
;                                then  x
;                                else  (factiter -(n,1) *(n,x))
;               in (factiter 4 1)"
;      5)
; (run "let x = 1
;        in  begin 
;              print(x);
;              set x =2;
;              print(x)
;            end"
;       5)
;(run "letrec 
;          noisy (x) = if null?(x)
;                      then 0
;                      else begin print(car(x)); 
;                                 (noisy cdr(x)) 
;                           end
;        in
;          begin 
;            spawn(proc (d) (noisy list(1,2,3,4,5)));
;            spawn(proc (d) (noisy list(6,7,8,9,10)));
;            print(100);
;            33
;          end"
;       5)
;(run "let buffer = 0
;      in  let producer = proc (n)
;                           letrec
;                             waite(k) = if zero?(k)
;                                       then begin
;                                              set buffer = n;
;                                              print(42)
;                                            end
;                                       else begin 
;                                              print(*(k,10));
;                                              (waite -(k,1))
;                                            end
;                           in (waite 5)
;          in let consumer = proc (d)
;                              letrec busywait (k) = if zero?(buffer)
;                                                    then begin
;                                                           print(*(k,100));
;                                                           (busywait -(k,1))
;                                                         end
;                                                    else buffer
;                               in (busywait 0)
;             in begin 
;                  spawn(proc (d) (producer 44));
;                  print(300);
;                  (consumer 86)
;                end"
;     10)
; (run "let x = 3
;      in let incrx = proc (id)
;                        proc (dummy)
;                          begin
;                            set x = -(x,1);
;                            print(x)
;                          end
;         in begin  
;              spawn((incrx 100));
;              spawn((incrx 200));
;              spawn((incrx 300))
;            end"
;     1)
(run "let x = 3
      in  let mut = mutex()
          in let incrx = proc (id)
                           proc (dummy)
                             begin 
                               wait(mut);
                               set x = -(x,1);
                               print(x);
                               signal(mut)
                             end
             in begin
                  spawn((incrx 100));
                  spawn((incrx 200));
                  spawn((incrx 300))
                end"
     1)

