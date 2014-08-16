#lang eopl
;(define occurs-free?
;  (lambda (var exp)
;    (cond 
;      ((symbol? exp) (eq? var exp))
;      ((eq? (car exp) 'lambda)
;       (and (not (eq? var (car (cadr exp))))
;            (occurs-free? var (caddr exp))))
;      (else (or (occurs-free? var (car exp))
;                (occurs-free? var (cadr exp)))))))
(define occurs-free?
  (lambda (var exp)
    (occurs-free/k var exp (end-cont))))
(define continuation continuation?
  (end-cont)
  (and-cont
   (v1 (lambda (x) #t))
   (cont continuation?))
  (or-cont 
(define occurs-free?
  (lambda (var exp cont)
    (
    



;(define remove-first
;  (lambda (s los)
;    (if (null? los)
;        '()
;        (if (eq? (car los) s)
;            (cdr los)
;            (cons (car los) (remove-first s (cdr los)))))))

;;data-structure
;(define-datatype continuation continuation?
;  (end-cont)
;  (rem-cont
;   (sym symbol?)
;   (cont1 continuation?)))
;(define apply-cont
;  (lambda (cont val)
;    (cases continuation cont
;      (end-cont ()
;                (begin
;                  (eopl:printf "End of computation.~%")
;                  (eopl:printf "This sentence should appear only once.~%")
;                  val))
;      (rem-cont (sym cont)
;                (apply-cont cont (cons sym val))))))
;;procedure
;(define end-cont
;  (lambda ()
;    (lambda (val)
;      (begin
;        (eopl:printf "End of computation.~%")
;        (eopl:printf "This sentence should appear only once.~%")
;        val))))
;(define rem-cont
;  (lambda (x cont)
;    (lambda (val)
;      (apply-cont cont (cons x val)))))
;(define apply-cont
;  (lambda (cont val)
;    (cont val)))
;(define remove-first
;  (lambda (s los)
;    (remove-first/k s los (end-cont))))
;(define remove-first/k  
;  (lambda (s los cont)
;    (if (null? los)
;        (apply-cont cont '())
;        (if (eq? (car los) s)
;            (apply-cont cont (cdr los))
;            (remove-first/k s (cdr los) (rem-cont (car los) cont))))))
;;inlined
;(define remove-first
;  (lambda (s los)
;    (remove-first/k s los   (lambda (val)
;                               (begin
;                                 (eopl:printf "End of computation.~%")
;                                 (eopl:printf "This sentence should appear only once.~%")
;                                 val)))))
;(define remove-first/k  
;  (lambda (s los cont)
;    (if (null? los)
;        (cont '())
;        (if (eq? (car los) s)
;            (cont (cdr los))
;            (remove-first/k s (cdr los) (lambda (val)
;                                          (cont (cons (car los) val))))))))
;;register
;(define s '())
;(define los '())
;(define cont '())
;(define remove-first
;  (lambda (x y)
;    (set! s x)
;    (set! los y)
;    (set! cont (lambda (val)
;                 (begin
;                   (eopl:printf "End of computation.~%")
;                   (eopl:printf "This sentence should appear only once.~%")
;                   val)))
;    (remove-first/k)))
;(define remove-first/k
;  (lambda ()
;    (if (null? los)
;        (apply-cont)
;        (if (eq? (car los) s)
;            (begin 
;              (set! los (cdr los))
;              (apply-cont))
;            (begin 
;              (set! cont (let ((temp (car los))
;                               (cen cont))
;                           (lambda (val)
;                                (cen (cons temp val)))))
;              (set! los (cdr los))
;              (remove-first/k))))))
;(define apply-cont
;  (lambda ()
;    (cont los)))




;(define list-sum
;  (lambda (loi)
;    (if (null? loi)
;        0
;        (+ (car loi)
;           (list-sum (cdr loi))))))
;(define list-sum
;  (lambda (loi)
;    (list-sum/k loi (end-cont))))
;(define list-sum/k
;  (lambda (loi cont)
;    (if (null? loi)
;        (apply-cont cont 0)
;        (list-sum/k (cdr loi) (add-cont (car loi) cont)))))
;(define-datatype continuation continuation?
;  (end-cont)
;  (add-cont
;   (v integer?)
;   (cont continuation?)))
;(define apply-cont
;  (lambda (cont val)
;    (cases continuation cont
;      (end-cont ()
;                (begin
;                  (eopl:printf "End of computation.~%")
;                  (eopl:printf "This sentence should appear only once.~%")
;                  val))
;      (add-cont (v cont)
;                (apply-cont cont (+ v val))))))
;(define list-sum
;  (lambda (loi)
;    (list-sum/k loi (end-cont))))
;(define list-sum/k
;  (lambda (loi cont)
;    (if (null? loi)
;        (apply-cont cont 0)
;        (list-sum/k (cdr loi) (add-cont (car loi) cont)))))
;(define end-cont
;  (lambda ()
;    (lambda (val)
;       (begin
;                  (eopl:printf "End of computation.~%")
;                  (eopl:printf "This sentence should appear only once.~%")
;                  val))))
;(define add-cont
;  (lambda (v cont)
;   (lambda (val)
;     (apply-cont cont (+ v val)))))
;(define apply-cont
;  (lambda (cont val)
;    (cont val)))
;(define list-sum
;  (lambda (loi)
;    (list-sum/k loi (lambda (val)
;       (begin
;                  (eopl:printf "End of computation.~%")
;                  (eopl:printf "This sentence should appear only once.~%")
;                  val)))))
;(define list-sum/k
;  (lambda (loi cont)
;    (if (null? loi)
;        (cont 0)
;        (list-sum/k (cdr loi) (lambda (val)
;                                (cont (+ (car loi) val)))))))
;(define cont '())
;(define loi '())
;(define list-sum
;  (lambda (x)
;    (set! loi x)
;    (set! cont (lambda (v)
;                 (begin
;                   (eopl:printf "End of computation.~%")
;                   (eopl:printf "This sentence should appear only once.~%")
;                   v)))
;    (list-sum/k)))
;(define list-sum/k
;  (lambda ()
;    (if (null? loi)
;        (begin 
;          (set! loi 0)
;          (apply-cont))
;        (begin
;          (let ((cent cont)
;                (temp (car loi)))
;            (set! cont (lambda (v)
;                         (cent (+ temp v))))
;            (set! loi (cdr loi))
;            (list-sum/k))))))
;(define apply-cont
;  (lambda ()
;    (cont loi)))






