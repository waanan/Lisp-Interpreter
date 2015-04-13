#lang racket

(require "tailform.rkt")

(define proc-k (gensym))

(define (anf-of-program pgm)
  (anf-of-exps (list pgm) (lambda (simples) (car simples))))

(define (new-simple-exp? exp)
  (or (number? exp)
      (boolean? exp)
      (symbol? exp)
      (and (eq? (car exp) '-) (new-simple-exp? (cadr exp)) (new-simple-exp? (caddr exp)))
      (and (eq? (car exp) 'zero?) (new-simple-exp? (cadr exp)))
      (eq? (car exp) 'proc)
      (and (eq? (car exp) 'sum) (every? new-simple-exp? (cdr exp)))
      (and (not (or (eq? (car exp) 'if)
                    (eq? (car exp) 'let)
                    (eq? (car exp) 'letrec)))
           (every? (new-simple-exp? (cdr exp))))))

(define (anf-of-simple-exp exp)
  (cond ((or (number? exp)
             (boolean? exp)
             (symbol? exp))
         exp)
        ((eq? (car exp) '-)
         (list '- (anf-of-simple-exp (cadr exp)) 
               (anf-of-simple-exp (caddr exp)) ))
        ((eq? (car exp) 'zero?)
         (list 'zero? (anf-of-simple-exp (cadr exp))))
        ((eq? (car exp) 'proc)
           (list 'proc (append (cadr exp) (list proc-k)) (cps-of-exp (caddr exp) proc-k)))
        ((eq? (car exp) 'sum)
         (cons 'sum (map anf-of-simple-exp (cdr exp))))
        (else (display "EXP Invalid While cps-of-simple-exp!\n"))))

(define (list-index pred exps)
  (define (index-help lst n)
    (cond ((null? lst) #f)
          ((pred (car lst)) n)
          (else (index-help (cdr lst) (+ n 1))))) 
  (index-help exps 0))
(define (list-ref lst n)
  (if (= n 0)
      (car lst)
      (list-ref (cdr lst) (- n 1))))
(define (list-set lst n x)
  (if (= n 0)
      (cons x (cdr lst))
      (cons (car lst)(list-set (cdr lst) (- n 1) x))))

(define (anf-of-exps exps builder)
  (let ((pos (list-index (lambda (exp) (not (simple-exp? exp))) exps)))
    (if (not pos)
        (builder (map anf-of-simple-exp exps))
        (let ((k (gensym))
              (k1 (list-ref exps pos)))
          (if (new-simple-exp? k1)
          (list 'let (list k )
                (anf-of-exps (list-set exps pos k) builder))))))
  
  

(define (cps-of-exp exp k-exp)
  (cond ((number? exp) (list k-exp exp))
        ((boolean? exp) (list k-exp exp))
        ((symbol? exp) (list k-exp exp))
        ((eq? (car exp) 'proc) 
           (list k-exp (list 'proc (append (cadr exp) (list proc-k)) (cps-of-exp (caddr exp) proc-k))))
        ((eq? (car exp) 'zero?) 
         (cps-of-zero?-exp (cdr exp) k-exp))
        ((eq? (car exp) '-) 
         (cps-of-diff-exp (cadr exp) (caddr exp) k-exp))
        ((eq? (car exp) 'sum)
         (cps-of-sum-exp (cdr exp) k-exp))
        ((eq? (car exp) 'if)
         (cps-of-if-exp (cadr exp) (caddr exp) (cadddr exp) k-exp))
        ((eq? (car exp) 'let)
         (cps-of-let-exp (cadr exp) (caddr exp) k-exp))
        ((eq? (car exp) 'letrec)
         (cps-of-letrec-exp (cadr exp) (caddr exp) k-exp))
        (else 
         (cps-of-call-exp exp k-exp))
        ))
      

(define (cps-of-call-exp exp k-exp)
  (anf-of-exps exp (lambda (simples) (append simples (list k-exp)))))
(define (cps-of-zero?-exp exp k-exp)
  (anf-of-exps exp
               (lambda (simples)
                 (list k-exp (list 'zero? (car simples))))))
(define (cps-of-diff-exp exp1 exp2 k-exp)
  (anf-of-exps (list exp1 exp2)
               (lambda (simples)
                 (list k-exp (list '- (car simples) (cadr simples))))))
(define (cps-of-sum-exp exps k-exp)
  (anf-of-exps exps (lambda (exps) (list k-exp (cons 'sum exps)))))
(define (cps-of-if-exp exp1 exp2 exp3 k-exp)
  (anf-of-exps (list exp1)
               (lambda (simples)
                 (let ((k (gensym)))
                   (list 'let (list (list k k-exp))
                                     (list 'if (car simples)
                                           (cps-of-exp exp2 k)
                                           (cps-of-exp exp3 k)))))))
(define (cps-of-let-exp bindings body k-exp)
  (let* ((vars (map car bindings))
         (vals-exps (map cadr bindings)))
    (anf-of-exps vals-exps (lambda (simples) 
                             (list 'let (map list vars simples) (cps-of-exp body k-exp))))))
;    (cps-of-exp (car vals-exps) (list 'proc (car vars) (cps-of-exp body proc-k)))))
(define (cps-of-letrec-exp bindings body k-exp)  
  (let* ((p-names (map car bindings))
         (p-varss (map cadr bindings))
         (p-bodies (map caddr bindings)))
    (list 'letrec (map list p-names (map (lambda (vars) (append vars (list proc-k))) p-varss)
                       (map (lambda (exp) (cps-of-exp exp proc-k)) p-bodies))
          (cps-of-exp body k-exp))))
         
         
         
         
         
         
  






