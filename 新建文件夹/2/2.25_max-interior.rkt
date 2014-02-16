#lang eopl
(define-datatype bintree bintree?
  (leaf-node
   (num integer?))
  (interior-node
   (key symbol?)
   (left bintree?)
   (right bintree?)))
(define (current-key bin)
    (cases bintree bin
    (leaf-node
     (num) #f)
    (interior-node
     (key left right) key)))
(define (val-interior bin)
  (cases bintree bin
    (leaf-node
     (num) num)
    (interior-node
     (key left right) (+ (val-interior left) (val-interior right)))))
(define (max-help bin x)
  (let ((val (val-interior bin)))
    (cases bintree bin
    (leaf-node
     (num) x)
    (interior-node
     (key left right) (cond ((> val (car x)) (max-help right
                                                       (max-help left (list val (list key)))))
                            ((= val (car x)) (max-help right
                                                       (max-help left (list val (cons key (cadr x))))))
                            (else (max-help right
                                            (max-help left x))))))))
(define (max-interior bin)
  (cadr (max-help bin '(-1024 ()))))
(define tree-1
  (interior-node 'foo (leaf-node 2) (leaf-node 3)))
(define tree-2
  (interior-node 'bar (leaf-node -1) tree-1))
(define tree-3
  (interior-node 'baz tree-2 (leaf-node 1)))









