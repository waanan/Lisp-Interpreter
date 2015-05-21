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