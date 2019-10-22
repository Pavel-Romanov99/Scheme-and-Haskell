;accumulate function
;op - операция
;nv - неутрална стойност
;a - b интервал
;term - правило
;next - правило при което прескачаме от едно число до друго пример а+1, а+3 и тн.
(define (accumulate op nv a b term next)
  (if (> a b)
      nv ;ако нямаме валиден интервал сумата е нула (дъно на рекурсията) т.е връщаме неутралната стойност
      (op (term a) ;правилото по което променяме а 
          (accumulate op nv (next a); [a, b] = a + [(next a), b] или [a+1, b]
                      b term next))))

;filter function of accumulate
(define (filter-accum p?
                      op nv
                      a b
                      term next)
  (cond((> a  b) nv)
       ((p? a) (op (term a) ;извършва операцията с обработения а
                   (filter-accum p?
                                 op nv
                                 (next a) b
                                 term
                                 next)))
       (else (filter-accum p?
                           op nv
                           (next a) b
                           term
                           next))))

;сумата на всички делители на едно число
(define (divisors-sum n)
  (accumulate + 0
              1 n
              (lambda (i)
               (if(zero? (remainder n i))
                        i
                        0)) ; взимаме всички числа, които са делители на n
              (lambda (i) (+ i 1)))) ;искаме прескачаме с 1, за да можем да инерираме през всички делители

;divisors sum using filter function
(define (divisors-sum2 n)
  (filter-accum (lambda (i)
                  (zero? (remainder n i)))
                + 0
                1 n
                (lambda (i) i)
                (lambda (i) (+ i 1))))

; !!n
(define (double-fact n)
  (if(zero? (remainder n 2))
     (accumulate * 1
                 2 n
                 (lambda (i) i)
                 (lambda (i) (+ i 2)))
     (accumulate * 1
                 1 n
                 (lambda (i) i)
                 (lambda (i) (+ i 2)))
     ))

;factorial
(define (fact n)
  (accumulate * 1
              1 n
              (lambda (i) i)
              (lambda (i) (+ i 1))))

;биномен коефициент
(define (binom n k )
  (/ (fact n) (* (fact k) (fact (- n k)))))

;степен на двойката
(define (power n)
  (accumulate * 1
              1 n
              (lambda (i) 2) ; за да правим степен на двойката искаме да ползваме само 2 за умножението
              (lambda (i) (+ i 1))))

;колко пъти е верен предиката ?р
;това е функцията, която ще ползваме като условие
(define (even1? n)
  (if(zero? (remainder n 2))
     #t
     #f))

(define (count p? a b)
  (filter-accum p?
                + 0
                a b
                (lambda (i) 1)
                (lambda (i) (+ i 1))))

;chech whether a number is prime
(define (prime? n)
  (if(equal? (filter-accum (lambda (i)
                  (zero? (remainder n i)))
                            + 0
                            1 n
                            (lambda (i) i)
                            (lambda (i) (+ i 1))
                            ) (+ 1 n))
     #t
     #f
     )) 
                            
;12 задача решение
(define (permutable? f g a b)
  (filter-accum even?
                (lambda (x y) (and x y))
                #t
                a b
                (lambda (k)
                  (= (twist k f g) (twist k g f)))
                (lambda (k) (+ k 1))))
;само трябва да напишем 11 задача 
                
                   
              