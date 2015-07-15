#lang racket

(define apply-one-subst
  (lambda (ty0 tvar ty1)
    (cond ((eq? ty0 'int) 'int)
          ((eq? ty0 'bool) 'bool)
          ((pair? ty0) (cons (apply-one-subst (car ty0) tvar ty1)
                             (apply-one-subst (cdr ty0) tvar ty1)))
          (else (if (= ty0 tvar) ty1 ty0)))))
(define apply-subst-to-type
  (lambda (ty subst)
    (cond ((eq? ty 'int) 'int)
          ((eq? ty 'bool) 'bool)
          ((pair? ty) (cons (apply-subst-to-type (car ty) subst)
                            (apply-subst-to-type (cdr ty) subst)))
          (else (let ((temp (assoc ty subst)))
                  (if temp
                      (cdr temp)
                      ty))))))
(define empty-subst (lambda () '()))
(define extend-subst
  (lambda (subst tvar ty)
    (cons (cons tvar ty)
          (map (lambda (p)
                 (let ((oldlhs (car p))
                       (oldrhs (cdr p)))
                   (cons oldlhs (apply-one-subst oldrhs tvar ty))))
               subst))))
(define unifier
  (lambda (ty1 ty2 subst exp)
    (let ((ty1 (apply-subst-to-type ty1 subst))
          (ty2 (apply-subst-to-type ty2 subst)))
      (cond ((eq? ty1 ty2) subst)
            ((integer? ty1) (if (no-occurrence? ty1 ty2)
                                (extend-subst subst ty1 ty2)
                                (report-on-occurrence-voilation ty1 ty2 exp)))
            ((integer? ty2) (if (no-occurrence? ty2 ty1)
                                (extend-subst subst ty2 ty1)
                                (report-on-occurrence-voilation ty2 ty1 exp)))
            ((and (pair? ty1) (pair? ty2)) (let ((subst (unifier (car ty1) (car (ty2) subst exp))))
                                             (let ((subst (unifier (cdr ty1) (cdr ty2) subst exp)))
                                               subst)))
            (else (report-unification-failure ty1 ty2 exp))))))

(define no-occurrence? 
  (lambda (tvar ty)
    (cond ((eq? ty 'int) #t)
          ((eq? ty 'bool) #t)
          ((pair? ty) (and (no-occurrence? tvar (car ty)) (no-occurrence? tvar (cdr ty))))
          (else (not (= tvar ty))))))
(define report-on-occurrence-voilation
  (lambda (ty1 ty2 exp) (display "NO Occurrence Voilation ") (display ty1) (display "  ") (display ty2) (display "  ") (display exp)))
(define report-unification-failure
  (lambda (ty1 ty2 exp) (display "NO Occurrence Voilation ") (display ty1) (display "  ") (display ty2) (display "  ") (display exp)))

                      