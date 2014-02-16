#lang eopl
(define-datatype rbtree rbtree?
  (leaf-node 
   (num integer?))
  (red-node 
   (rbl-subtree rbtree?)
   (rbr-subtree rbtree?))
  (blue-node
   (rbtrees rbtrees?)))
(define (rbtrees? rbtrees)
  (or (null? rbtrees)
      (and (rbtree? (car rbtrees))
           (rbtrees? (cdr rbtrees)))))
(define (rb-path-help red-blue-tree x)
  (cases rbtree red-blue-tree
    (leaf-node 
     (num) x)
    (red-node 
     (rbl-subtree rbr-subtree) (list 'red 
                                     (rb-path-help rbl-subtree (+ x 1))
                                     (rb-path-help rbr-subtree (+ x 1))))
    (blue-node
     (rb-trees) (if (null? rb-trees)
                    '(blue)
                    (cons 'blue (map (lambda (tree) (rb-path-help tree x)) rb-trees))))))  
(define (rb-path rbtree)
  (rb-path-help rbtree 0))
(define test-1 (blue-node '()))
(define test-2 (red-node (leaf-node 1) (leaf-node 2)))
(define test-3 (leaf-node 4))
(define test-4 (red-node (leaf-node 3) (blue-node (list (leaf-node 4) (leaf-node 5)))))
(define test-5 (red-node (leaf-node 3) (blue-node (list (leaf-node 4) (red-node (leaf-node 2) (leaf-node 2))))))
(define test-6 (blue-node (list (red-node (leaf-node 2) (blue-node (list (leaf-node 2)))) (leaf-node 2) (leaf-node 2))))






