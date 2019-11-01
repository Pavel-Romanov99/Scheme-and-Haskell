;check if odd
(define (odd?? x)
  (if(zero? (remainder x 2))
            #f
            #t))

;check if even
(define (even?? x)
  (if(equal? (odd?? x ) #t)
     #f
     #t))

;grade 
(define (grade x)
  (cond
    ((< x 30) 2)
    ((and (< x 50)  (> x 30) 3))
    (else 4)))

;define factoriel
(define (fact n)
  (if(= n 1)
     1
     (* n (fact (- n 1)))))

;define fibonacci
(define (fib n)
  (cond
    ((= n 1) 0)
    ((= n 2) 1)
    (else (+ (fib (- n 1)) (fib (- n 2))))))

;sum of an interval
(define (interval a b)
  (if(= a b)
     a
     (+ a (interval (+ a 1) b))))

;pow
(define (pow1 x n)
  (if(even?? n)
     (expt (expt x (quotient n 2)) 2)
     (* x (expt (expt x (quotient n 2)) 2))))

;count digits
(define (count-digit n d)
  (define(for n counter)
  (cond
    ((= n 0) counter)
    ((= d (remainder n 10)) 
        (for (quotient n 10) (+ counter 1)))
    (else (for (quotient n 10) counter))))
  (for n 0))

;succ
(define (succ x)
  (+ x 1))

;pred
(define (pred x)
  (- x 1))

;safe-div
(define (safe-div x)
  (if(even?? x)
     (quotient x 2)
     x)
  )

;iterative factorial
(define (fact-iter n)
  (define (for n counter result)
    (if(= counter n )
       result
       (for n (+ counter 1) (* result counter))))
  (for n 1 1))

;iterative fibonacci
(define (fib-iter n)
  (define(for a b counter )
    (if(= counter 1)
       b
       (for b (+ a b) (- counter 1))
       ))
  (for 0 1 n ))

;reverse
(define (reverse1 n)
  (define (for n res)
    (if(= n 0)
       res
       (for (quotient n 10) (+ (* res 10) (remainder n 10)))
       ))
  (for n 0))

;palindrome
(define (palindrome1 n)
  (if(= n (reverse1 n))
     #t
     #f))


;sum of dividers
(define (div-sum1 n)
  (define (for a n result)
    (cond
      ((= n a) result)
      ((= (remainder n a) 0)
          (for (+ a 1) n (+ result a )))
      (else (for ( + a 1) n result))))
  (for 1 n n))

;perfect number
(define (perfect? n)
  (if (= n (- (div-sum1 n) n ))
      #t
      #f))

;prime number
(define (prime? n)
  (if(= (div-sum1 n) (+ 1 n))
     #t
     #f))

;convert to binary
(define (toBinary n)
  (if(= n 0)
     '()
     (cons(remainder n 2)
          (toBinary (quotient n 2)))))

;binary to decimal
(define (toDecimal n)
  (define (for n counter result)
    (if(= n 0)
       result
      (for (quotient n 10) (+ counter 1) (+ result (* (remainder n 10) (expt 2 counter))) )))
  (for n 0 0))

;accumulate
(define (accumulate op nv a b term next)
  (if (> a b)
      nv
      (op (term a)
          (accumulate op nv (next a) b term next))))

(define (filter-accum p? op nv a b term next)
  (cond ((> a b) nv)
        ((p? a) (op (term a)
                    (filter-accum p? op nv (next a) b term next)))
        (else       (filter-accum p? op nv (next a) b term next))))

;factoriel of only odd or even numbers
(define (!! n)
  (if(zero?(remainder n 2))
     (accumulate * 1 2 n (lambda (i) i) (lambda (i) (+ i 2)))
     (accumulate * 1 1 n (lambda (i) i) (lambda (i) (+ i 2)))))

;binomial coefficient
(define (fact-acc n)
  (accumulate * 1 1 n
              (lambda(i) i)
              (lambda(i) (+ i 1))))
(define (binom n k)
  (/ (fact-acc n) (* (fact-acc k) (fact-acc (- n k)))))


;pow of 2 to the n
(define (power1 n)
  (accumulate * 1
              1 n
              (lambda(i) 2)
              (lambda(i) (+ i 1))))

;sum of all divisors of n
(define (divisors n)
  (filter-accum (lambda(i)
                  (zero? (remainder n i)))
                + 0
                1 n
                (lambda(i) i)
                (lambda(i) (+ i 1))))

;checks how many times the statement p? is correct for the numbers [a to b]
(define (counter p? a b)
  (filter-accum p?
              + 0
              a b
              (lambda(i) 1)
              (lambda(i) (+ i 1))))

;check for prime number
(define (prime??? a)
  (if(= (divisors a) (+ 1 a))
     #t
     #f))

;higher order testing
(define (double f x)
  (f x x))

;constantly c
(define (constantly c)
  c)

(define (forever-21 f)
  (f 21))

;flip
(define (flip f)
  f)

(define (op f a b)
  (f b a))

;отрицание на предикат
(define (complement p?)
  (if(equal? p? #t)
     #f
     #t))

(define (less-than-5 x)
  (if(< x 5)
     #t
     #f))
  


(define (1+ x)
  (+ x 1))

(define (square x)
  (* x x))

(define (foo f x)
  (f x))

;twist
(define (twist k f g)
  (if(= k 0)
     (lambda(x) x)
     (lambda (x) (f (g ((twist (- k 2) f g) x))))))

;counter even numbers between [a b]
(define (count-even a b)
  (define (for counter)
  (if(= a b)
    counter
    (if(even? a)
    (+ (count-even (+ a 1) b ) 1 )
    (count-even (+ a 1) b ))
    ))
  (for 0))
      
;permutable?
(define (permutable? a b f g)
  (define k (count-even a b))
  (if(= (twist k f g) (twist k g f) ) 
     #t
     #f))

