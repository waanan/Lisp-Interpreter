#lang eopl
;;th-id
(define cur-th-id '())
(define th-ids '())

(define alloc-new-id
  (lambda ()
    (let ((an (alloc-help-1 0 th-ids)))
    (set! th-ids (alloc-help-2 0 th-ids))
    an)))
(define del-id
  (lambda (x)
    (set! th-ids (del-id-help x th-ids))))
(define del-id-help
  (lambda (x ids)
    (if (= x (car ids))
        (cdr ids)
        (cons (car ids) (del-id-help x (cdr ids))))))
(define alloc-help-1
  (lambda (n x)
    (if (or (null? x)
            (not (= n (car x))))
        n
        (alloc-help-1 (+ n 1) (cdr x)))))
(define alloc-help-2
  (lambda (n x)
    (if (or (null? x)
            (not (= n (car x))))
        (cons n x)
        (cons n (alloc-help-2 (+ n 1) (cdr x))))))   

;;queue
(define the-ready-queue '())
(define the-final-answer '())
(define the-max-time-slice '())
(define the-time-remaining '())
(define the-send-queue '())
(define the-get-queue '())


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
  (lambda (th th-id)
    (set! the-ready-queue
          (enqueue the-ready-queue (cons th th-id)))))
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
                   (set! cur-th-id (cdr first-ready-thread))
                   ((car first-ready-thread)))))))
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
  (send-exp
   (exp1 expression?)
   (exp2 expression?))
  (get-exp)
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
     ("send" expression "to" expression)
     send-exp)
    (expression
     ("get" "(" ")")
     get-exp)
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
  (wait-cont
   (cont continuation?))
  (signal-cont
   (cont continuation?))
  (send1-cont
   (exp1 expression?)
   (env environment?)
   (cont continuation?))
  (send2-cont
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
  (lambda (cont val)
    (if (time-expired?)
        (begin
          (place-on-ready-queue!
           (lambda () (apply-cont cont val))
           cur-th-id)
;          (display th-ids)
          (run-next-thread))
        (begin 
          (decrement-timer!)
          (cases continuation cont
            (end-main-thread-cont ()
                                  (set-final-answer! val)
                                  (del-id cur-th-id)
                                  (run-next-thread))
            (end-sub-thread-cont ()
                                 (del-id cur-th-id)
                                 (run-next-thread))
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
            (wait-cont (saved-cont)
                       (wait-for-mutex
                        (expval->mutex val)
                        (lambda () (apply-cont saved-cont (num-val 52)))))
            (signal-cont (saved-cont)
                         (signal-mutex
                          (expval->mutex val)
                          (lambda () (apply-cont saved-cont (num-val 53)))))
            (send1-cont (exp1 saved-env saved-cont)
                        (value-of/k exp1 saved-env (send2-cont val saved-cont)))
            (send2-cont (val1 saved-cont)
                        (send-mes val1 val saved-cont))
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
                           (apply-cont saved-cont (list-help (cons val expvals)))
                           (value-of/k (car exps) env (list-cont (cons val expvals) (cdr exps) env saved-cont))))
            
            (print-cont (saved-cont)
                        (display val)
                        (newline)
                        (apply-cont saved-cont (num-val 10)))
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
            
            (spwan-cont (saved-cont)
                        (let ((proc1 (expval->proc val))
                              (new-id (alloc-new-id)))
                          (place-on-ready-queue!
                           (lambda ()
                             (apply-procedure/k proc1
                                                (list (num-val 28))
                                                (end-sub-thread-cont)))
                           new-id)
                          (apply-cont saved-cont (num-val new-id))))
            
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
                                        (rands-cont val1 (append rands (list val)) (cdr rest-rands) env cont)))))))))
;;help-function
(define list-iter
  (lambda (expvals result)
    (if (null? expvals)
        result
        (list-iter (cdr expvals) (ls-val (cons-val (car expvals) result)))))) 
(define list-help
  (lambda (expvals)
    (list-iter expvals (ls-val (null-val)))))
(define send-mes
  (lambda (val1 val2 saved-cont)
    (let ((n (expval->num val2)))
      (if (id-in-queue? n the-get-queue)
          (set! the-get-queue
                (del-in-get-list the-get-queue
                                 (lambda (x) (= n (cdr x)))
                                 (lambda (x)
                                   (lambda ()
                                     (apply-cont x val1)))))
          (set! the-send-queue (cons (cons val1 n) the-send-queue)))
      (apply-cont saved-cont (num-val 100)))))
(define get-mes
  (lambda (saved-cont)
    (if (id-in-queue? cur-th-id the-send-queue)
        (let ((val (del-in-send2 the-send-queue (lambda (x) (= cur-th-id (cdr x))))))
          (set! the-send-queue (del-in-send the-send-queue (lambda (x) (= cur-th-id (cdr x)))))
          (apply-cont saved-cont val))
        (begin 
          (set! the-get-queue (cons (cons saved-cont cur-th-id) the-get-queue))
          (run-next-thread)))))
(define id-in-queue?
  (lambda (n queue)
    (cond ((null? queue)
           #f)
          ((= (cdr (car queue)) n)
           #t)
          (else
            (id-in-queue? n (cdr queue))))))
(define del-in-send
  (lambda (queue op)
    (if (null? queue)
        '()
        (if (op (car queue))
            (cdr queue)
            (cons (car queue) (del-in-send (cdr queue) op))))))
(define del-in-send2
  (lambda (queue op)
    (if (null? queue)
        '()
        (if (op (car queue))
            (car (car queue))
            (del-in-send2 (cdr queue) op)))))
(define del-in-get-list
  (lambda (queue op1 op2)
    (if (null? queue)
        '()
        (if (op1 (car queue))
            (begin
              (place-on-ready-queue!
               (op2 (car (car queue))) 
               (cdr (car queue)))
              (cdr queue))
            (cons (car queue) (del-in-get-list (cdr queue) op1 op2))))))         
           
(define wait-for-mutex
  (lambda (m th)
    (cases mutex m
      (a-mutex (ref-to-closed? ref-to-wait-queue)
               (cond 
                 ((deref ref-to-closed?)
                  (setref! ref-to-wait-queue
                           (enqueue (deref ref-to-wait-queue) (cons th cur-th-id)))
                  (run-next-thread))
                 (else
                  (setref! ref-to-closed? #t)
                  (th)))))))
(define signal-mutex
  (lambda (m th)
    (cases mutex m
      (a-mutex (ref-to-closed? ref-to-wait-queue)
               (let ((closed? (deref ref-to-closed?))
                     (wait-queue (deref ref-to-wait-queue)))
                 (if closed?
                     (if (empty? wait-queue)
                         (setref! ref-to-closed? #f)
                         (dequeue wait-queue
                                  (lambda (first-waiting-th other-waiting-ths)
                                    (place-on-ready-queue!
                                     (car first-waiting-th)
                                     (cdr first-waiting-th))
                                    (setref! ref-to-wait-queue
                                             other-waiting-ths))))
                     (begin (display "signal open mutex")
                            (newline)))
                 (th))))))

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
    (set! the-send-queue '())
    (set! the-get-queue '())
    (set! th-ids '())
    (set! cur-th-id (alloc-new-id))
    (cases program pgm
      (a-program (exp1)
                 (value-of/k exp1 (init-env) (end-main-thread-cont))))))
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
      (print-exp (exp1)
                 (value-of/k exp1 env (print-cont cont)))
      (mutex-exp ()
                 (apply-cont cont (mutex-val (new-mutex))))
      (wait-exp (exp1)
                (value-of/k exp1 env (wait-cont cont)))
      (signal-exp (exp1)
                  (value-of/k exp1 env (signal-cont cont)))
      (send-exp (exp1 exp2)
                (value-of/k exp1 env (send1-cont exp2 env cont)))
      (get-exp ()
               (get-mes cont))
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
      (spwan-exp (exp1)
                 (value-of/k exp1 env (spwan-cont cont)))
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
;               in (fact 4)
;      5")

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
;                             wait(k) = if zero?(k)
;                                       then begin
;                                              set buffer = n;
;                                              print(42)
;                                            end
;                                       else begin 
;                                              print(*(k,10));
;                                              (wait -(k,1))
;                                            end
;                           in (wait 5)
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
;(run "let x = 3
;      in  let mut = mutex()
;          in let incrx = proc (id)
;                           proc (dummy)
;                             begin 
;                               wait(mut);
;                               set x = -(x,1);
;                               print(x);
;                               signal(mut)
;                             end
;             in begin
;                  spawn((incrx 100));
;                  spawn((incrx 200));
;                  spawn((incrx 300))
;                end"
;     1)
;(run "begin
;        spawn(proc (x) send 5 to 2);
;        spawn(proc (x) print(-(5,get())))
;      end"
;     4)
(run "begin
        spawn(proc (x) print(-(5,get())));
        spawn(proc (x) send 5 to 1)
      end"
     2)
