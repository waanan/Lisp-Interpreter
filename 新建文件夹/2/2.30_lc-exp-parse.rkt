#lang eopl
(define identifier?
  (lambda (x) (and (symbol? x) (not (eq? x 'lambda)))))
(define list-of
  (lambda (pred)
    (lambda (lst)
      (if (null? lst)
          #t
          (and (pair? lst) (pred (car lst)) ((list-of pred) (cdr lst)))))))
(define-datatype lc-exp lc-exp?
  (var-exp
   (var identifier?))
  (lambda-exp
   (bound-vars (list-of identifier?))
   (body lc-exp?))
  (app-exp
   (rator lc-exp?)
   (rand lc-exp?)))
(define parse-exp
  (lambda (datum)
   (cond ((symbol? datum) (var-exp datum))
         ((pair? datum)
          (cond ((and (eq? (car datum) 'lambda) (= (length datum) 3) (list? (cadr datum))) (lambda-exp (cadr datum) (parse-exp (caddr datum))))
                ((and (reduce-tf datum symbol?) (> (length datum) 2)) (eopl:error "~s are all symbols" datum))
                ((= (length datum) 2) (app-exp (parse-exp (car datum)) (parse-exp (cadr datum))))
                ((eq? (car datum) 'lambda) (eopl:error "~s are inappropriate with lambda" datum))
                (else (eopl:error "~s are wrong list" datum))))
         (else (eopl:error "~s are wrong" datum)))))
(define reduce-tf 
  (lambda (lst pred)
    (if (null? lst)
        #t
        (and (pred (car lst))
         (reduce-tf (cdr lst) pred)))))




              
              
    
    
