#lang eopl
(define-datatype stack stack?
  (empty-stack)
  (extend-stack
   (val (lambda (x) (or (number? x) (symbol? x) (list? x))))
   (old-stack stack?)))
(define push
  (lambda (push-stack val)
    (extend-stack val push-stack)))
(define pop
  (lambda (pop-stack)
    (cases stack pop-stack
      (empty-stack 
       () "empty-stack")
      (extend-stack
       (val old-stack) (list old-stack
                              val)))))
(define top
  (lambda (top-stack)
    (cases stack top-stack
      (empty-stack 
       () "empty-stack")
      (extend-stack
       (val old-stack) val))))
(define empty-stack?
  (lambda (emp-stack)
    (cases stack emp-stack
      (empty-stack 
       () #t)
      (extend-stack
       (val old-stack) #f))))
(define test (empty-stack))
(define test2 (extend-stack 2 (empty-stack)))  

      

    
    
    
    
    
    
    
    
    
    