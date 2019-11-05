;exercises on deep lists
(define a '((1 (2)) (((3) 4) (5 (6)) () (7)) 8))
(define b '(1 2 3 4 5))

(define (atom? x) (and (not (null? x)) (not (pair? x)))) ;вертикалното ни дъно (до достигане на атом)

;count the number of atoms in a deep list
(define (num_atoms lst)
  (cond
    ((null? lst) 0)
    ((atom? lst) 1)
    (else (+ (num_atoms (car lst)) (num_atoms (cdr lst))))))

;merge all atoms of a deep list into one
(define (merge* lst)
  (cond
    ((null? lst) '())
    ((atom? lst) (list lst))
    (else (append (merge* (car lst)) (merge* (cdr lst))))))

;return the sum of a deep list
(define (deep-sum ls)
  (cond ((null? ls) 0)
        ((atom? ls)          ; only add atoms
         (if (number? ls)
             ls
             0))     ; only add numbers
        (else (+ (deep-sum (car ls)) ; advance recursion on both car and car
                 (deep-sum (cdr ls))))))

;помощна функция
(define (deep-foldr nv term op l)
  (cond ((null? l) nv)
        ((atom? l) (term l))
        (else (op (deep-foldr nv term op (car l))
                  (deep-foldr nv term op (cdr l))))
        ))

;count atoms using deep-foldr
(define (count-atoms lst)
  (deep-foldr 0 (lambda (x)  1 ) + lst))

;merge using deep-foldr
(define (merge1 lst)
  (deep-foldr '() list append lst))

;sum of a deep list using deep-foldr
(define (sum1 lst)
  (define (for result)
    (deep-foldr 0 (lambda (x) (+ result x)) + lst))
  (for 0))

;Как работи deep-foldr?
;пуска себе си рекурсивно за всеки елемент на дълбокия списък
;при достигане на вертикално дъно (атоми) прилага term
;и събира резултатите с op

;finds an element in a list
(define (find_el lst x)
  (cond
    ((null? lst) #f)
    ((equal? (car lst) x) #t)
    ((list? (car lst))
     (or (find_el (car lst) x) (find_el (cdr lst) x)))
     (else (find_el (cdr lst) x))))

;average sum in a deep list
(define (total_sum lst)
  (define (for result)
    (deep-foldr 0 (lambda(x) (+ result x)) + lst))
  (for 0))

(define (num_atoms lst)
  (cond
    ((null? lst) 0)
    ((atom? lst) 1)
    (else (+ (num_atoms (car lst)) (num_atoms (cdr lst))))))

(define (average_sum lst)
  (/ (total_sum lst) (num_atoms lst)))


          



      
