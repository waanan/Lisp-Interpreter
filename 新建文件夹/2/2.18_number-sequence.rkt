#lang eopl
(define number->sequence
  (lambda (x)
    (list x '() '())))
(define current-element
  (lambda (seq)
    (car seq)))
(define move-to-left
  (lambda (seq)
    (let ((current-elem (car seq))
          (left-part (cadr seq))
          (right-part (caddr seq)))
      (if (eq? left-part '())
          (eopl:error "left part is empty")
          (list (car left-part) (cdr left-part) (cons current-elem right-part))))))
(define move-to-right
  (lambda (seq)
    (let ((current-elem (car seq))
          (left-part (cadr seq))
          (right-part (caddr seq)))
      (if (eq? right-part '())
          (eopl:error "right part is empty")
          (list (car right-part) (cons current-elem left-part) (cdr right-part)))))) 
(define test '(5 (4 3 2 1) (6 7 8 9)))
