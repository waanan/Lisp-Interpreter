#lang eopl

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

(define scanner-spec
  '((white-sp (whitespace) skip)
    (comment ("%" (arbno (not #\newline))) skip)
    (identifier (letter (arbno (or letter digit))) symbol)
    ;    (boolean ("#" (or "t" "f")) symbol) 
    (number ((or "" "-") digit (arbno digit)) number)))

(define-datatype environment environment?
  (empty-env)
  (extend-env
   (vars (list-of symbol?))
   (vals (list-of expval?))
   (saved-env environemnt?))
  (extend-env**
   (p-names (list-of symbol?))
   (p-vars (list-of (list-of symbol?)))
   (p-bodies (list-of tfexp?))))
   
   (define list-of
  (lambda (pred)
    (lambda (lst)
      (if (null? lst)
          #t
          (and (pair? lst)
               (pred (car lst))
               ((list-of pred) (cdr lst)))))))