(define lst (list 1 2 33 4 5 6 77 88 9))

;list length
(define (length* lst1)
  (define (for counter)
    (if(null? lst1)
       counter
       (+ (length* (cdr lst1)) 1)))
  (for 0))

;list sum
(define (sum* lst)
  (define (for result)
    (if(null? lst)
       result
       (+ (car lst) (sum* (cdr lst)))))
  (for 0))

;return last element of a list
(define (last* lst)
  (if(null? (cdr lst))
     (car lst)
     (last* (cdr lst))))

;return the n-th element of a list
(define (n-th lst counter)
  (if(= counter 1)
     (car lst)
     (n-th (cdr lst) (- counter 1))))

;concatenate lists
(define (concat lst1 lst2)
  (if(null? lst1)
     lst2
     (cons
      (car lst1)
      (concat (cdr lst1) lst2))))

;map function
(define (map* f lst )
  (if(null? lst)
     lst
     (cons (f (car lst))
     (map* f (cdr lst)))))

(define(1+ x)
  (+ x 1))

;filter a list
(define (filter* p lst)
  (cond ((null? lst) lst)
        ((p (car lst))
         (cons
           (car lst)
           (filter* p (cdr lst))))
        (else (filter* p (cdr lst)))))

;partition a list
(define (partition* p lst lst2)
  (cond
    ((null? lst) lst (list lst2))
    ((p ( car lst))
       (cons (car lst) (partition* p (cdr lst) lst2)))
       (else (partition* p (cdr lst) (cons lst2 (car lst))))))

    

     
