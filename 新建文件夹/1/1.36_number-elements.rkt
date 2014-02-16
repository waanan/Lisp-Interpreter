(define help
  (lambda (y)
    (if (null? y)
        '()
        (cons (list (+ (caar y) 1) (cadar y)) (help (cdr y))))))
(define g
  (lambda (x y)
    (cons x (help y))))
(define number-elements
  (lambda (lst)
    (if (null? lst)
        '()
        (g  (list 0 (car lst)) (number-elements (cdr lst))))))
