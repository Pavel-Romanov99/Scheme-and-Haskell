;Работа с дълбоки списъци
(define (atom? x) (and (not (null? x)) (not (pair? x))))
(define l '((1 (2)) (((3) 4) (5 (6)) () (7)) 8))

;1.Да се преброят атомите на даден дълбок списък
(define (count-atoms lst)
  (cond
    ((null? lst) 0) ;хоризонтално обхождане - до достигане на празен списък
    ((atom? lst) 1) ;вертикално обхождане - до достигане на друг атом
    (else (+ (count-atoms (car lst)) (count-atoms (cdr lst))))))

;2.Да се направи списък от дълбок списък
(define (flatten* lst)
  (define (for lst new)
    (cond
      ((null? lst) '())
      ((atom? lst) (list lst))
      (else (append (flatten* (car lst)) (flatten* (cdr lst))))))
  (for lst '()))

;3.Reverse a deep list
(define (deep-reverse lst)
  (cond
    ((null? lst) '())
    ((atom? lst) lst)
    (else (append (deep-reverse (cdr lst)) (list (deep-reverse (car lst)))))))

;4.Map function for deep lists
(define (plus-5 x)
  (+ x 5))

(define (map* f lst)
  (cond
    ((null? lst) '())
    ((atom? lst) (f lst))
    (else (cons (map* f (car lst)) (map* f (cdr lst))))))

