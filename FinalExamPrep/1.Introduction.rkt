;1. check if a number is odd or even
(define (even?? x)
  (if(zero? (remainder x 2))
     #t
     #f))

(define (odd?? x)
  (if(= (remainder x 2) 1)
     #t
     #f))

;2.Get the grade of a student
(define (grade x)
  (cond
    ((< x 60) 2)
    ((and (> x 60) (< x 100)) 3)
    ((and (> x 100) (< x 140)) 4)
    ((and (> x 140) (< x 180)) 5)
    (else 6)))

;3.Factorial
(define (factorial x)
  (if(= x 1)
     1
     (* x (factorial (- x 1)))))

;4.Fibonacci
(define (fibonacci x)
  (cond
    ((zero? x) 0)
    ((= x 1) 1)
    (else (+ (fibonacci (- x 2)) (fibonacci (- x 1))))))

;5.Sum of all numbers in an interval
(define (sum_interval a b)
  (cond
    ((= a b) b)
    (else (+ b (sum_interval a (- b 1))))))

;6.How many times a digit is found in a number
(define (countMeetings num d)
  (define (for num1 counter)
    (cond ((= num1 0 ) counter)
          ((= d (remainder num1 10)) (for (quotient num1 10) (+ counter 1)))
          (else (for (quotient num1 10) counter)
)))
  (for num 0))

;7. ++ and --
(define (succ x)
  (+ x 1))

(define (prev x )
  (- x 1 ))

;8.safe-div
(define (save-div x)
  (if(even?? x)
     (/ x 2)
     x))

;9.is root
(define (sqrt* x)
  (* x x))

(define (is_root? x)
 (if( zero? (- (+ (* (sqrt* x) 3 ) (* x -2)) 1))
  #t
  #f))