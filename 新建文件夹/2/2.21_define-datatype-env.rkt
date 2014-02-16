#lang eopl
(define-datatype daty-env env?
  (empty-env)
  (extend-env
   (var symbol?)
   (val (lambda (x) (or (number? x) (symbol? x) (list? x))))
   (old-env env?)))
(define apply-env
  (lambda (env search-var)
    (cases daty-env env
      (empty-env 
       () (report-no-such-bounding search-var))
      (extend-env
       (var val old-env)
       (if (eq? var search-var)
           val
           (apply-env old-env search-var))))))
(define report-no-such-bounding
  (lambda (x)
    (eopl:error 'apply-env "no ~s found" x)))
(define has-binding 
  (lambda (env search-var)
    (cases daty-env env
      (empty-env 
       () #f)
      (extend-env
       (var val old-env)
       (if (eq? var search-var)
           #t
           (has-binding old-env search-var))))))
(define test1 (empty-env))
(define test2 (extend-env 'a 2 test1))
(define test3 (extend-env 's 3 test2))