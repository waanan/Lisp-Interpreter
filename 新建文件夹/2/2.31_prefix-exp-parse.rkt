#lang eopl
(define-datatype prefix-exp prefix-exp?
  (const-exp
   (num integer?))
  (diff-exp
   (operand1 prefix-exp?)
   (operand2 prefix-exp?)))
;;old-deal
;(define (deal lst)
;  (if (> (length lst) 2)
;      (let ((fir (car lst))
;            (sec (cadr lst))
;            (thir (caddr lst)))
;        (if (and (eq? fir '-) (not (eq? sec '-)) (not (eq? thir '-)))
;            (cons (list '- sec thir) (deal (cdddr lst)))
;            (cons fir (deal (cdr lst)))))
;      lst))
;(define (parse lst)
;  (if (= (length lst) 3)
;      lst 
;      (parse (deal lst))))

;;new-deal
(define (add lst receiver)
  (cond ((and (null? lst) (= (length receiver) 3)) receiver)
        ((null? lst)
         (let ((one (car receiver))
               (fir (cadr receiver))
               (sec (caddr receiver)))
           (add '() (cons (list '- fir one) (cdddr receiver)))))
        ((> (length receiver) 1)
         (let ((one (car lst))
               (fir (car receiver))
               (sec (cadr receiver)))
           (if (and (eq? sec '-) (not (eq? one '-)) (not (eq? fir '-)))
               (add (cdr lst) (cons (list '- fir one) (cddr receiver)))
               (add (cdr lst) (cons one receiver)))))
        (else (add (cdr lst) 
                   (cons (car lst) receiver)))))
(define (parse lst)
  (if (= (length lst) 3)
      lst
      (reverse (add lst '()))))
        
        
(define parse-prefix-exp
  (lambda (pre-exp)
    (cond ((integer? pre-exp) (const-exp pre-exp))
          ((pair? pre-exp) 
           (let ((exp (parse pre-exp)))
               (diff-exp (parse-prefix-exp (cadr exp)) (parse-prefix-exp (caddr exp))))))))