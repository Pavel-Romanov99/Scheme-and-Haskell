;return the middle two digits in a number
;number of digits
(define (num_digits n)
  (define (for n counter)
    (if(= n 0)
       counter
       (for (quotient n 10) (+ counter 1))))
  (for n 0))

;we make a list of the digits of n
(define (list_n n)
  (if(= n 0)
     '()
     (cons (remainder n 10) (list_n (quotient n 10)))))

;remove last element from a list
(define (remove_last* lst)
  (if(null? (cdr lst))
     '()
     (cons (car lst) (remove_last* (cdr lst)))))

;remove first element from a list
(define removeFirst
  (lambda (input)
    (cond
      ((list? (car input)) (cons (removeFirst (car input)) (cdr input)))
      (else (cdr input)))))

;remove both first and last
(define (remove_both lst)
  (if(null? (cdr lst))
     #f
     (removeFirst (remove_last* lst))))

(define (middle-digits n)
     (define (for counter lst)
       (cond
         ((odd? counter) #f)
         ((= counter 2) lst)
         (else (for (- counter 2) (remove_both lst)))))
     (for (num_digits n) (list_n n)))
