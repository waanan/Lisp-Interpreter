#lang eopl
;;old style
;(define occur-free?
; (lambda (var exp)
;    (cond
;      ((symbol? exp) (eq? var exp))
;      ((eq? (car exp) 'lambda)
;       (and 
;        (not (eq? var (caadr exp)))
;        (occur-free? var  (caddr exp))))
;      (else (or 
;             (occur-free? var (car exp))
;             (occur-free? var (cadr exp)))))))
;;with interfaces
(define occur-free?
  (lambda (var exp)
    (cond 
      ((var-exp? exp) (eq? var (var-exp->var exp)))
      ((lambda-exp? exp) 
       (and (not (eq? var (lambda-exp->bound-var exp)))
            (occur-free? var (lambda-exp->body exp))))
      (else 
       (or (occur-free? var (app-exp->rator exp))
           (occur-free? var (app-exp-rand exp)))))))
(define var-exp? symbol?)
(define var-exp->var 
  (lambda (x) x))
(define lambda-exp?
  (lambda (lst)
    (eq? (car lst) 'lambda)))
(define lambda-exp->bound-var
  (lambda (lst)
    (caadr lst)))
(define lambda-exp->body
  (lambda (lst)
    (caddr lst)))
(define app-exp->rator
  (lambda (lst)
    (car lst)))
(define app-exp-rand
  (lambda (lst)
    (cadr lst)))

















