#lang eopl

;(define subst
;  (lambda (new old slist)
;    (if (null? slist)
;        '()
;         (cons (subst-in-s-exp new old (car slist))
;               (subst new old (cdr slist))))))
;(define subst-in-s-exp
;  (lambda (new old sexp)
;    (if (symbol? sexp)
;        (if (eq? sexp old) new sexp)
;        (subst new old sexp))))
;(define subst
;  (lambda (new old slist)
;    (subst/k new old slist (end-cont))))
;(define subst/k
;  (lambda (new old slist cont)
;    (if (null? slist)
;        (apply-cont cont '())
;        (subst-in-a-exp new old (car slist) (a-cont new old (cdr slist) cont)))))
;(define subst-in-a-exp
;  (lambda (new old sexp cont)
;    (if (symbol? sexp)
;        (if (eq? sexp old)
;            (apply-cont cont new)
;            (apply-cont cont sexp))
;        (subst/k new old sexp cont))))
;(define-datatype continuation continuation?
;  (end-cont)
;  (a-cont
;   (new (lambda (x) #t))
;   (old (lambda (x) #t))
;   (exp (lambda (x) #t))
;   (cont continuation?))
;  (b-cont
;   (x (lambda (x) #t))
;   (cont continuation?)))
;(define apply-cont
;  (lambda (cont val)
;    (cases continuation cont
;      (end-cont ()
;                (begin
;                  (eopl:printf "End of computation.~%")
;                  (eopl:printf "This sentence should appear only once.~%")
;                  val))
;      (a-cont (new old exp cont)
;              (subst/k new old exp (b-cont val cont)))
;      (b-cont (x cont)
;              (apply-cont cont (cons x val))))))
;(define subst
;  (lambda (new old slist)
;    (subst/k new old slist (end-cont))))
;(define subst/k
;  (lambda (new old slist cont)
;    (if (null? slist)
;        (apply-cont cont '())
;        (subst-in-a-exp new old (car slist) (a-cont new old (cdr slist) cont)))))
;(define subst-in-a-exp
;  (lambda (new old sexp cont)
;    (if (symbol? sexp)
;        (if (eq? sexp old)
;            (apply-cont cont new)
;            (apply-cont cont sexp))
;        (subst/k new old sexp cont))))
;(define apply-cont
;  (lambda (cont val)
;    (cont val)))
;(define end-cont 
;  (lambda ()
;    (lambda (val)
;      (begin
;        (eopl:printf "End of computation.~%")
;        (eopl:printf "This sentence should appear only once.~%")
;        val))))
;(define a-cont 
;  (lambda (new old exp cont)
;    (lambda (val)
;      (subst/k new old exp (b-cont val cont)))))
;(define b-cont 
;  (lambda (x cont)
;    (lambda (val)
;      (apply-cont cont (cons x val)))))
;(define subst
;  (lambda (new old slist)
;    (subst/k new old slist (lambda (val)
;                             (begin
;                               (eopl:printf "End of computation.~%")
;                               (eopl:printf "This sentence should appear only once.~%")
;                               val)))))
;(define subst/k
;  (lambda (new old slist cont)
;    (if (null? slist)
;        (cont '())
;        (subst-in-a-exp new old 
;                        (car slist) 
;                        (lambda (val)
;                          (subst/k new 
;                                   old 
;                                   (cdr slist) 
;                                   (lambda (v)
;                                     (cont (cons val v)))))))))
;(define subst-in-a-exp
;  (lambda (new old sexp cont)
;    (if (symbol? sexp)
;        (if (eq? sexp old)
;            (cont new)
;            (cont sexp))
;        (subst/k new old sexp cont))))
(define new '())
(define old '())
(define slist '())
(define cont '())
(define subst
  (lambda (x y z)
    (set! new x)
    (set! old y)
    (set! slist z)
    (set! cont (lambda (val)
                             (begin
                               (eopl:printf "End of computation.~%")
                               (eopl:printf "This sentence should appear only once.~%")
                               val)))
    (subst/k)))
(define subst/k
  (lambda ()
    (if (null? slist)
        (cont '())
        (let ((cent cont)
              (temp (cdr slist)))
          (set! cont (lambda (val)
                       (set! cont (lambda (v)
                                     (cent (cons val v))))
                       (set! slist temp)
                       (subst/k)))
          (set! slist (car slist))
          (subst-in-a-exp)))))
(define subst-in-a-exp
  (lambda ()
    (if (symbol? slist)
        (if (eq? slist old)
            (cont new)
            (cont slist))
        (subst/k))))






;(define occurs-free?
;  (lambda (var exp)
;    (cond 
;      ((symbol? exp) (eq? var exp))
;      ((eq? (car exp) 'lambda)
;       (and (not (eq? var (car (cadr exp))))
;            (occurs-free? var (caddr exp))))
;      (else (or (occurs-free? var (car exp))
;                (occurs-free? var (cadr exp)))))))
;(define occurs-free?
;  (lambda (var exp)
;    (occurs-free/k var exp (end-cont))))
;(define-datatype continuation continuation?
;  (end-cont)
;  (and-cont
;   (v1 (lambda (x) #t))
;   (cont continuation?))
;  (or1-cont 
;   (var1 (lambda (x) #t))
;   (exp1 (lambda (x) #t))
;   (cont continuation?))
;  (or2-cont
;   (v1 (lambda (x) #t))
;   (cont continuation?)))
;(define occurs-free/k
;  (lambda (var exp cont)
;    (cond
;      ((symbol? exp) (apply-cont cont (eq? var exp)))
;      ((eq? (car exp) 'lambda)  (occurs-free/k var 
;                                               (caddr exp) 
;                                               (and-cont (not (eq? var (car (cadr exp)))) cont)))
;      (else (occurs-free/k var 
;                           (car exp)
;                           (or1-cont var (cadr exp) cont))))))
;(define apply-cont
;  (lambda (cont val)
;    (cases continuation cont
;       (end-cont ()
;                (begin
;                  (eopl:printf "End of computation.~%")
;                  (eopl:printf "This sentence should appear only once.~%")
;                  val))
;      (and-cont (v1 cont)
;                (apply-cont cont (and v1 val)))
;      (or1-cont (var exp cont)
;                (occurs-free/k var exp (or2-cont val cont)))
;      (or2-cont (v1 cont)
;                (apply-cont cont (or val v1))))))
;(define occurs-free?
;  (lambda (var exp)
;    (occurs-free/k var exp (end-cont))))
;(define occurs-free/k
;  (lambda (var exp cont)
;    (cond
;      ((symbol? exp) (apply-cont cont (eq? var exp)))
;      ((eq? (car exp) 'lambda)  (occurs-free/k var 
;                                               (caddr exp) 
;                                               (and-cont (not (eq? var (car (cadr exp)))) cont)))
;      (else (occurs-free/k var 
;                           (car exp)
;                           (or1-cont var (cadr exp) cont))))))
;(define apply-cont
;  (lambda (cont val)
;    (cont val)))
;(define end-cont 
;  (lambda ()
;    (lambda (val)
;      (begin
;        (eopl:printf "End of computation.~%")
;        (eopl:printf "This sentence should appear only once.~%")
;        val))))
;(define and-cont 
;  (lambda (v1 cont)
;    (lambda (val)
;      (apply-cont cont (and v1 val)))))
;(define or1-cont 
;  (lambda (var exp cont)
;    (lambda (val)
;      (occurs-free/k var exp (or2-cont val cont)))))
;(define or2-cont 
;  (lambda (v1 cont)
;    (lambda (val)
;      (apply-cont cont (or val v1)))))  
;(define occurs-free?
;  (lambda (var exp)
;    (occurs-free/k var exp (lambda (val)
;                             (begin
;                               (eopl:printf "End of computation.~%")
;                               (eopl:printf "This sentence should appear only once.~%")
;                               val)))))
;(define occurs-free/k
;  (lambda (var exp cont)
;    (cond
;      ((symbol? exp) (cont (eq? var exp)))
;      ((eq? (car exp) 'lambda)  (occurs-free/k var 
;                                               (caddr exp)
;                                               (lambda (val)
;                                                 (cont (and (not (eq? var (car (cadr exp)))) val)))))                                               
;      (else (occurs-free/k var 
;                           (car exp)
;                           (lambda (v1)
;                             (occurs-free/k var (cadr exp) (lambda (val)
;                                                             (cont (or val v1))))))))))
;(define var '())
;(define exp '())
;(define cont '())
;(define occurs-free?
;  (lambda (x y)
;    (set! var x)
;    (set! exp y)
;    (set! cont (lambda (val)
;                 (begin
;                   (eopl:printf "End of computation.~%")
;                   (eopl:printf "This sentence should appear only once.~%")
;                   val)))
;    (occurs-free/k)))
;(define occurs-free/k
;  (lambda ()
;    (cond
;      ((symbol? exp) (cont (eq? var exp)))
;      ((eq? (car exp) 'lambda) (let ((cent cont)
;                                     (temp (car (cadr exp))))
;                                 (set! cont (lambda (val)
;                                                 (cent (and (not (eq? var temp)) val))))
;                                 (set! exp (caddr exp)) 
;                                 (occurs-free/k)))                                               
;      (else 
;       (let ((cent cont)
;             (temp (cadr exp)))
;         (set! cont (lambda (v1)
;                      (set! exp temp)
;                      (set! cont (lambda (val)
;                                                (cent (or val v1))))
;                      (occurs-free/k)))
;         (set! exp (car exp))
;         (occurs-free/k))))))




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