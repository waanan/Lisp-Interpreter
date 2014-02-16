#lang eopl
(define number->bintree 
  (lambda (x)
    (list 'root x '() '())))
(define at-leaf?
  (lambda (bintree)
    (null? (cdr bintree))))
(define at-root?
  (lambda (bintree)
    (eq? (car bintree) 'root)))
(define current-element
  (lambda (bintree)
    (cadr bintree)))
(define insert-to-left
  (lambda (x bintree)
    (let ((father (car bintree))
          (left-child (caddr bintree))
          (right-child (cadddr bintree))
          (curr-elem (current-element bintree)))
      (list father curr-elem (list x left-child '()) right-child))))
(define insert-to-right
  (lambda (x bintree)
    (let ((father (car bintree))
          (left-child (caddr bintree))
          (right-child (cadddr bintree))
          (curr-elem (current-element bintree)))
      (list father curr-elem left-child (list x right-child '())))))
(define move-to-left
  (lambda (bintree)
    (let ((father bintree)
          (left-child (caddr bintree)))
      (cons father left-child))))
(define move-to-right
  (lambda (bintree)
    (let ((father bintree)
          (right-child (cadddr bintree)))
      (cons father right-child))))
(define move-up 
  (lambda (bintree)
    (car bintree)))
;;test
(define root (number->bintree 13))
(define t1 (insert-to-right 14
                            (insert-to-left 12
                                            (number->bintree 13))))
















