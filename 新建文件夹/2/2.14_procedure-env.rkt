#lang eopl
(define empty-env
  (lambda ()
    (lambda (search-var)
      (cond ((eq? search-var 'empty-env?) #t)
             ((eq? (car search-var) 'has-binding?) #f) 
            (#t (report-no-binding-found search-var))))))
(define (report-no-binding-found var)
    (eopl:error var "no such var found"))

(define extend-env
  (lambda (saved-var saved-val saved-env)
    (lambda (search-var)
      (cond ((eq? search-var saved-var)   saved-val)
            ((eq? search-var 'empty-env?) #f)
            ((and (pair? search-var) (eq? (car search-var) 'has-binding?) (eq? (cadr search-var) saved-var)) #t) 
            (#t (apply-env saved-env search-var))))))

(define apply-env
  (lambda (env search-var)
    (env search-var)))
(define empty-env?
  (lambda (env)
    (apply-env env 'empty-env?)))
(define had-binding? 
  (lambda (env search-var)
    (apply-env env (list 'has-binding? search-var))))
(define test (extend-env 'a 3 (extend-env 'as 2 (empty-env))))