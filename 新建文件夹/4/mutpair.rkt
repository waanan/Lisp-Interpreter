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
             (newref (proc-val (procedure body (nth nameless-env (car n))))))))))
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
  (setcar-exp
   (exp1 expression?)
   (exp2 expression?))
  (setcdr-exp
   (exp1 expression?)
   (exp2 expression?))
  
  (newarr-exp
   (n integer?)
   (x expression?))
  (dearr-exp
   (arr expression?)
   (n integer?))
  (setarr-exp
   (arr expression?)
   (n integer?)
   (exp expression?))
  (arr-length-exp
   (arr expression?))
  
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
                           (number? (cdr x)))
                      (and (number? (cdr x)) (eq? (car x) 'mutable))))))
  (let-exp
   (lst1 (list-of symbol?))
   (lst2 (list-of expression?))
   (body expression?))
  (nameless-let-exp
   (exps (list-of expression?))
   (body expression?))
  
  (letmutable-exp
   (lst1 (list-of symbol?))
   (lst2 (list-of expression?))
   (body expression?))
  (nameless-letmutable-exp
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
  
  (setdyn-exp
   (var symbol?)
   (exp1 expression?)
   (body expression?))
  (nameless-setdyn-exp
   (n number?)
   (exp1 expression?)
   (body expression?))
  
  (assign-exp
   (exp1 symbol?)
   (exp2 expression?))
  (nameless-assign-exp
   (exp1 number?)
   (exp2 expression?))
  (beg-exp
   (exps (list-of expression?)))
  
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


;;scan-parse
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
     ("cond" (arbno expression "==>" expression) "end")
     cond-exp)
    (expression
     ("let" (arbno identifier "=" expression) "in" expression)
     let-exp)
    (expression
     ("let*" (arbno identifier "=" expression) "in" expression)
     let*-exp)
    (expression
     ("letmutable" (arbno identifier "=" expression) "in" expression)
     letmutable-exp)
        
    (expression
     ("letrec" (arbno identifier "(" identifier ")" "=" expression) "in" expression)
     letrec-exp)
    (expression
     ("unpack" (arbno identifier) "=" expression "in" expression)
     unpack-exp)
    
    (expression
     ("set" identifier "=" expression)
     assign-exp)
    (expression
     ("begin" (separated-list expression ";") "end")
     beg-exp)
    (expression
     ("setdynamic" identifier "=" expression "during" expression)
     setdyn-exp)
    
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
     ("setcar!" "(" expression "," expression ")")
     setcar-exp)
    (expression
     ("setcdr!" "(" expression "," expression ")")
     setcdr-exp)
    
    (expression
     ("newarray" "(" number "," expression ")")
     newarr-exp)
    (expression
     ("arrayref" "(" expression "," number ")")
     dearr-exp)
    (expression
     ("arrayset" "(" expression "," number "," expression ")")
     setarr-exp)
    (expression
     ("arrlength" "(" expression ")")
     arr-length-exp)
     
    
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
    (initialize-store!)
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
   (proc proc?))
  (ref-val
   (ref reference?))
  (array-val
   (arr array?))
  (mutpair-val
   (pair mutpair?)))
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
(define expval->mutpair
  (lambda (val)
    (cases expval val
      (mutpair-val (pair) pair)
      (else (report-expval-extractor-error 'mutpair val)))))
(define expval->ref
  (lambda (val)
    (cases expval val
      (ref-val (ref) ref)
      (else (report-expval-extractor-error 'ref val)))))
(define expval->array
  (lambda (val)
    (cases expval val
      (array-val (arr) arr)
      (else (report-expval-extractor-error 'arr val)))))

(define report-expval-extractor-error 
  (lambda (x y)
    (eopl:error x "~s extract error" y)))

(define-datatype proc proc?
  (procedure
   (body expression?)
   (saved-nameless-env nameless-environment?)))

;old cons
;(define cons-accept?
;  (lambda (x)
;    (or (lst-exp? x)
;        (expval? x))))          
;(define-datatype lst-exp lst-exp?
;  (null-exp)
;  (cons-exp
;   (first cons-accept?)
;   (second cons-accept?)))

;array
(define-datatype array array?
  (a-array
   (length integer?)
   (start reference?)))
(define new-array
  (lambda (n x)
    (let ((start (newref x)))
      (begin (array-iter (- n 1) x)
             (a-array n start)))))
(define array-iter
  (lambda (n x)
   (cond ((not (= n 0))
          (begin (newref x)
                 (array-iter (- n 1) x))))))
(define array-length
  (lambda (arr)
    (cases array arr
      (a-array (length start)
               length))))
(define array-ref
  (lambda (arr n)
    (cases array arr 
      (a-array (length start)
               (cond ((>= n length)
                      (eopl:error 'array-ref "over the array length ~s" length))
                     ((< n 0)
                      (eopl:error 'array-ref "~s is a error array length" n))
                     (else (deref (+ start n))))))))
(define array-set
  (lambda (arr n val)
    (cases array arr
      (a-array (length start)
               (cond ((>= n length)
                      (eopl:error 'array-ref "over the array length ~s" length))
                     ((< n 0)
                      (eopl:error 'array-ref "~s is a error array length" n))
                     (else (setref! (+ start n) val)))))))

(define-datatype mutpair mutpair?
  (a-pair
   (left-loc reference?)
   (right-loc reference?)))
(define make-pair
  (lambda (val1 val2)
    (a-pair (newref val1)
            (newref val2))))
(define left
  (lambda (p)
    (cases mutpair p
      (a-pair (left-loc right-loc)
              (deref left-loc)))))
(define right
  (lambda (p)
    (cases mutpair p
      (a-pair (left-loc right-loc)
              (deref right-loc)))))
(define setleft
  (lambda (p val)
    (cases mutpair p
      (a-pair (left-loc right-loc)
              (setref! left-loc val)))))
(define setright
  (lambda (p val)
    (cases mutpair p
      (a-pair (left-loc right-loc)
              (setref! right-loc val)))))

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
      (setcar-exp (exp1 exp2)
                  (setcar-exp (translation-of exp1 senv)
                              (translation-of exp2 senv)))
      (setcdr-exp (exp1 exp2)
                  (setcdr-exp (translation-of exp1 senv)
                              (translation-of exp2 senv)))
      
      (newarr-exp (n exp)
                  (newarr-exp n (translation-of exp senv)))
      (dearr-exp (arr n)
                 (dearr-exp (translation-of arr senv) n))
      (setarr-exp (arr n exp)
                  (setarr-exp (translation-of arr senv)
                              n
                              (translation-of exp senv)))
      (arr-length-exp (arr)
                      (arr-length-exp (translation-of arr senv)))
      
      
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
      (letmutable-exp (vars exps body)
               (nameless-letmutable-exp
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
     
      (setdyn-exp (var exp1 body)
                  (nameless-setdyn-exp
                   (apply-senv senv var)
                   (translation-of exp1 senv)
                   (translation-of body senv)))
      (assign-exp (var exp1)
                  (nameless-assign-exp (apply-senv senv var)
                              (translation-of exp1 senv)))
      (beg-exp (exps)
               (beg-exp (map (lambda (exp) (translation-of exp senv)) exps)))
      
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
      (proc-exp (vars body)
                (nameless-proc-exp
                 (translation-of body
                                 (append (reverse vars) senv))))
      (call-exp (rator rands)
                (call-exp
                 (translation-of rator senv)
                 (map (lambda (rand) (translation-of rand senv)) rands)))
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
      (nameless-var-exp (n) (deref 
                             (let ((post (apply-nameless-env nameless-env n)))
                               (if (and (pair? post) (eq? (car post) 'mutable))
                                   (cdr post)
                                   post))))
                                       
      (construct-exp (exp1 exp2)
                     (let ((val1 (value-of exp1 nameless-env))
                           (val2 (value-of exp2 nameless-env)))
                       (mutpair-val (make-pair val1 val2))))
      (car-exp (exp1)
               (let ((val1 (value-of exp1 nameless-env)))
                 (let ((p1 (expval->mutpair val1)))
                   (left p1))))
      
      (cdr-exp (exp1)
               (let ((val1 (value-of exp1 nameless-env)))
                 (let ((p1 (expval->mutpair val1)))
                   (right p1))))
      (setcar-exp (exp1 exp2)
                  (let ((val1 (value-of exp1 nameless-env))
                        (val2 (value-of exp2 nameless-env)))
                    (let ((p (expval->mutpair val1)))
                      (begin (setleft p val2)
                             (num-val 43)))))
      (setcdr-exp (exp1 exp2)
                  (let ((val1 (value-of exp1 nameless-env))
                        (val2 (value-of exp2 nameless-env)))
                    (let ((p (expval->mutpair val1)))
                      (begin (setright p val2)
                             (num-val 44)))))
      
      (newarr-exp (n exp)
                  (new-array n (value-of exp nameless-env)))
      (dearr-exp (arr n)
                 (array-ref (value-of arr nameless-env) n))
      (setarr-exp (arr n exp)
                  (array-set (value-of arr nameless-env)
                             n
                             (value-of exp nameless-env)))
      (arr-length-exp (arr)
                      (array-length (value-of arr nameless-env)))
      
;      (list-exp (exps)
;                (list->cons exps nameless-env))
;      (null?-exp (exp1)
;                 (let ((val1 (value-of exp1 nameless-env)))
;                   (cases lst-exp val1
;                     (null-exp () 
;                               (bool-val #t))
;                     (cons-exp (exp exp2)
;                               (bool-val #f))))) 
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
      (nameless-letmutable-exp (exps body)
                        (letmutable->val exps body nameless-env nameless-env))
      (nameless-let*-exp (exps body)
                (let*->val exps body nameless-env))      
      (nameless-letrec-exp (bodys letrec-body)
                  (value-of letrec-body (extend-nameless-env bodys nameless-env)))
      (nameless-unpack-exp (exp1 exp2)
                  (nameless-unpack->val exp1 exp2 nameless-env))
      
      (nameless-setdyn-exp (var-post exp1 body)
                           (let ((val (value-of exp1 nameless-env))
                                 (n (apply-nameless-env nameless-env var-post)))
                             (let ((saved-val (value-of (nameless-var-exp var-post) nameless-env)))
                               (if (and (pair? n) (eq? (car n) 'mutable))
                                   (begin (setref! (cdr n) val)
                                          (let ((return-val (value-of body nameless-env)))
                                            (setref! (cdr n) saved-val)
                                            return-val))
                                   (begin (setref! n val)
                                          (let ((return-val (value-of body nameless-env)))
                                            (setref! n saved-val)
                                            return-val))))))
                                       
      (nameless-assign-exp (var-post exp1)
                               (let ((val (value-of exp1 nameless-env))
                                     (n (apply-nameless-env nameless-env var-post)))
                                   (if (and (pair? n) (eq? (car n) 'mutable))
                                       (begin (setref! (cdr n) val)
                                              (num-val 42))
                                       (eopl:error 'assign-exp "assign to unmutable var ~s" var-post))))
      (beg-exp (exps)
               (beg->val exps nameless-env))
      
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
      (call-exp (rator rands)
;               (eopl:printf "~s" rator) (newline)
                (let ((proc (expval->proc (value-of rator nameless-env))))
                  (apply-procedure proc rands nameless-env)))
      (else (eopl:error 'value-of "Invalid value ~s in ~s" exp nameless-env)))))


;;value-of help func
;(define list->cons
;  (lambda (exps nameless-env)
;    (if (null? exps)
;        (null-exp)
;        (cons-exp (value-of (car exps) nameless-env)
;                  (list->cons (cdr exps) nameless-env)))))
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
          (let->val (cdr exps) body nameless-env (extend-nameless-env (newref val) new-env))))))
(define letmutable->val
  (lambda (exps body nameless-env new-env)
    (if (null? exps)
        (value-of body new-env)
        (let ((val (value-of (car exps) nameless-env)))
          (letmutable->val (cdr exps) body nameless-env (extend-nameless-env (cons 'mutable (newref val)) new-env))))))
(define let*->val
  (lambda (exps body nameless-env)
    (if (null? exps)
        (value-of body nameless-env)
        (let ((val (value-of (car exps) nameless-env)))
          (let*->val (cdr exps) body (extend-nameless-env (newref val) nameless-env))))))
(define nameless-unpack->val
  (lambda (exp1 exp2 nameless-env)
    (cond ((expval->bool (value-of (null?-exp exp1) nameless-env)) (value-of exp2 nameless-env))
          (else (let ((val (value-of (car-exp exp1) nameless-env)))
                  (nameless-unpack->val (cdr-exp exp1) exp2 (extend-nameless-env (newref val) nameless-env))))))) 
(define beg->val
  (lambda (exps nameless-env)
    (if (= (length exps) 1)
        (value-of (car exps) nameless-env)
        (begin
          (value-of (car exps) nameless-env)
          (beg->val (cdr exps) nameless-env)))))

(define apply-procedure
  (lambda (proc1 rands nameless-env)
    (cases proc proc1
      (procedure (body saved-nameless-env)
                     (apply-proc-help body rands nameless-env saved-nameless-env)))))
(define apply-proc-help
  (lambda (body rands nameless-env old-env)
    (if (null? rands)
        (value-of body old-env)
        (apply-proc-help body (cdr rands) nameless-env (extend-nameless-env (newref (value-of (car rands) nameless-env)) old-env)))))
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











