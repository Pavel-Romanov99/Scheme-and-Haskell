;1.Reverse a number
(define (reverse* num)
  (define (for num result)
    (cond
      ((= num 0) result)
      (else (for (quotient num 10) (+ (* result 10) (remainder num 10))))))
  (for num 0))

;2.Check for palindrome
(define (palindrome? num)
  (if(= num (reverse* num))
        #t
        #f))

;3.Divisors sum
(define (div-sum num)
  (define (for num div result)
    (cond
      ((= num div) (+ result div))
      ((zero? (remainder num div)) (for num (+ div 1) (+ result div)))
      (else (for num (+ div 1) result))))
  (for num 1 0))

;4.Perfect number
(define (perfect? num)
  (if(= num (- (div-sum num) num))
     #t
     #f))

;5.Prime
(define (prime? num)
  (if(= (div-sum num) (+ 1 num))
     #t
     #f))

;6.Check if digits of a number a increasing
(define (increasing? num)
  (define (for num) 
    (cond
      ((= num 0) #t)
      ((> (remainder num 10) (remainder num 100)) (for (quotient num 10)))
      (else #f)))
  (for num))

;7.dec to binary
(define (dec-to-bin n)
  (if (< n 2)
      n
      (+ (* 10 (dec-to-bin (quotient n 2)))
         (remainder n 2))))

;8.Binary to decimal
(define (pow x n)
   (cond
     ((zero? n) 1)
     ((= n 1) x)
     (else (* x (pow x (- n 1))))))

(define (bin-to-dec n)
  (define (for num result stepen)
    (cond
      ((= num 0) result)
      (else (for (quotient num 10) (+ result (* (remainder num 10) (pow 2 stepen))) (+ stepen 1)))))
  (for n 0 0))

;9.Decimal to binary
(define (dec-to-bin n)
  (define (for num result 