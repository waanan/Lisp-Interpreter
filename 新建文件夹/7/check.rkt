#lang eopl

(define report-unequal-types
  (lambda (ty1 ty2 exp)
    (eopl:error 'check-equal-types!
                "Types didn't match: ~s != ~a in~%~a"
                ty1 ty2 exp)))
(define check-equal-type!
  (lambda (ty1 ty2 expr)
    (cond ((not (equal? ty1 ty2))
           (report-unequal-types ty1 ty2 expr)))))

(define init-tenv
  (lambda () '()))

(define extend-tenv
  (lambda (var type tenv)
    (cons (cons var type) tenv)))

(define apply-tenv
  (lambda (tenv var)
    (if (null? tenv)
        (eopl:error 'Untyped-Symbol! "Symbol ~a don't have a type in TENV" var)
        (if (equal? var (caar tenv))
            (cdar tenv)
            (apply-tenv (cdr tenv) var)))))


(define type-of-program
  (lambda (pgm)
    (type-of pgm (init-tenv))))

(define type-of
  (lambda (exp tenv)
    (cond
      ((number? exp) 'int)
      ((symbol? exp) (apply-tenv tenv exp))
      ((boolean? exp) 'bool)
      ((equal? (car exp) '-)
       (let ((ty1 (type-of (cadr exp) tenv))
             (ty2 (type-of (caddr exp) tenv)))
         (check-equal-type! ty1 'int (cadr exp))
         (check-equal-type! ty2 'int (caddr exp))
         'int))
      ((equal? (car exp) 'zero?)
       (let ((ty1 (type-of (cadr exp) tenv)))
         (check-equal-type! ty1 'int (cadr exp))
         'bool))
      ((equal? (car exp) 'if)
       (let ((ty1 (type-of (cadr exp) tenv))
             (ty2 (type-of (caddr exp) tenv))
             (ty3 (type-of (cadddr exp) tenv)))
         (check-equal-type! ty1 'bool (cadr exp))
         (check-equal-type! ty2 ty3 exp)
         ty2))
      ((equal? (car exp) 'let)
       (let ((exp1-type (type-of (cadadr exp) tenv)))
         (type-of (caddr exp) (extend-tenv (caadr exp) exp1-type tenv))))
      ((equal? (car exp) 'proc)
       (let ((result-type (type-of (caddr exp)
                                   (extend-tenv (caadr exp) (cadadr exp) tenv))))
         (cons (cadadr exp) result-type)))
      ((equal? (car exp) 'letrec)
       (let ((ty1 (type-of (cadr (cdddr exp)) (extend-tenv (caddr exp) (cons (cadar (cdddr exp)) (cadr exp))
                                                           (extend-tenv (caar (cdddr exp)) (cadar (cdddr exp)) tenv)))))
         (check-equal-type! ty1 (cadr exp) (cadr (cdddr exp)))
         (type-of (caddr (cdddr exp)) (extend-tenv (caddr exp) (cons (cadar (cdddr exp)) (cadr exp)) tenv))))
      (else 
       (let ((rator-type (type-of (car exp) tenv))
             (rand-type (type-of (cadr exp) tenv)))
         (if (pair? rator-type)
             (begin 
               (check-equal-type! (car rator-type) rand-type (cadr exp))
               (cdr rator-type))
             (eopl:error 'Rator-Not-PROC-Type! " Rator ~a has a type ~a,NOT PROC!" (car exp) rator-type)))))))
































