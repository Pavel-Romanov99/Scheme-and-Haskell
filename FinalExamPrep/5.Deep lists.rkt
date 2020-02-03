;Deep lists
(define (atom? x) (and (not (null? x)) (not (pair? x))))
(define l '((1 (2)) (((3) 4) (5 (6)) () (7)) 8))

;1.Да се преброят атомите на даден дълбок списък'
(define (count-atoms lst)
  (cond
    ((null? lst) 0) ;ако е празен списъка
    ((atom? lst) 1) ;ако е атом
    (else (+ (count-atoms (car lst)) (count-atoms (cdr lst))))))

;2.Да се съберат всички елементи на дълбок списък
(define (flatten lst)
  (cond
    ((null? lst) '())
    ((atom? lst) (list lst))
    (else (append (flatten (car lst)) (flatten (cdr lst))))))