; digit counter recursive
(define (count-digits n d)
  (cond (( < n 10) (if (= n d) 1 0))
        ((= d (remainder n 10))
         (+ 1 (count-digits (quotient n 10) d)))
        (else (count-digits
               (quotient n 10) d))))

; digit counter iterative
(define (count-digits* n d)
  (define (for n counter)
    (cond ((= n 0) counter)
          ((= d (remainder n 10))
           (for (quotient n 10)
             (+ counter 1)))
          (else (for (quotient n 10) counter)
)))
(if (= n 0)
    (if (= d 0) 1 0)
    (for n 0)))

;fibonacci iterative
  (define (fibonacci n)
  (define (fib-iter a b counter)
    (if(= counter 0)
       b
       (fib-iter (+ a b) a (- counter 1))))
    (fib-iter 1 0 n))

;reverse number iterative
(define (reverse* n)
  (define (for n res)
(if (= n 0)
    res
    (for (quotient n 10) (+ (* res 10) (remainder n 10))))) 
  (for n 0))

;chech palindrome
(define (palindrome* n)
  (if(= n (reverse* n))
        #t
        #f))