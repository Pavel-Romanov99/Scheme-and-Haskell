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

;1.Произведение на всички числа по-малки от х със същата четност
(define (n!! x)
  (if(zero? (remainder x 2))
     (accumulate * 1
                 2 x
                 (lambda(i) i)
                 (lambda(i) (+ i 2)))
     (accumulate * 1
                 1 x
                 (lambda(i) i)
                 (lambda(i) (+ i  2)))))
;2.Биномен коефициент
(define (factorial n)
  (accumulate * 1
              1 n
              (lambda(i) i)
              (lambda(i) (+ i 1))))

(define(binom n k)
  (/ (factorial n) (* (factorial k) (factorial (- n k)))))

;3. 2 на стенен n
(define (pow2 n)
  (accumulate * 1
              1 n
              (lambda(i) 2)
              (lambda(i) (+ i 1))))

;4. Sum of dividers
(define (div-sum n)
  (filter-accum
   (lambda(i) (zero? (remainder n i)))
   + 0
   1 n
   (lambda(i) i)
   (lambda(i) (+ i 1))))

;5.Колко пъти е верен предикат
(define (count p? a b)
  (filter-accum
   (lambda(i) (p? i))
   + 0
   a b
   (lambda(i) 1)
   (lambda(i) (+ i 1))))

;6. All and any
(define (all? p? a b)
  (accumulate
   (lambda(x y) (and x y)) ;this is the operation искаме да е логическо и
   ;тоест ако едно от числата не отговаря на предиката то целия израз е лъжа
   #t ; default стойност
   a b ; от а до б
   (lambda(i) (p? i)) ; гледаме дали отговаря на предиката
   (lambda(i) (+ i 1))))

(define (any? p? a b)
  (accumulate
   (lambda(x y) (or x y)) ; операцията е логическо или
    #f ; стойността е лъжа
    a b ; от а до б
    (lambda(i) (p? i)) ; ако едно число отговаря на предиката, то
    ;целият израз е истина
    (lambda(i) (+ i 1))))

;7.Check for prime number
(define (prime? x)
  (if( = (div-sum x) (+ 1 x))
     #t
     #f))

;8.Define forever 21
(define (constantly c)
  (lambda(x) c))

;9.Размяна на аргументите на двуместна операция
(define (flip op)
  (lambda(x y) (op y x)))

(define f (flip -))

;10.По даден предикат и аргумент връща неговото отрицание
(define (complement p?)
  (lambda(x)
    (not (p? x))))

(define (less-than-5? x) (< x 5))
(define f (complement less-than-5?))

;11.Twist
(define (twist* k f g)
  ; Композицията на функции е нова функция
  (define (compose f g) (lambda (x) (f (g x))))
  (accumulate compose
              (lambda (x) x) ; коя е неутралната стойност на композицията на функции?
              1 k
              (lambda (i) (if (odd? i) f g))
              (lambda (i) (+ i 1))))


(define foo (twist* 4 (lambda (x) (+ x 1)) (lambda (x) (* x x))))

;12.Да видим дали ф г .. е равно на г ф ... за всяко число к в интервала [a,b]
(define (permutable? f g a b)
  (filter-accum
   even? ;за четните к
   (lambda(x y) (and x y)) ; operation
   #t ;default стойност
   a b ; от а до б
   (lambda(k) ;проверка дали е вярно
     (= ((twist* k f g) k)
        ((twist* k g f) k)))
   (lambda(k) (+ k 1))))

;13.Композиция f(f(x))
(define (composition f)
  (lambda(x) (f (f x))))

(define (plus-5 x)
  (+ x 5))

(define foo* (composition plus-5))

;14.Kомпозиция f(g(x))
(define (comp f g)
  (lambda(x) (f (g x))))

(define foo** (comp (lambda(x) (* x 5)) (lambda(x) (+ x 2))))

;15.Прилагане на функция n на брой пъти
(define (repeat f n)
  (if (zero? n) ;ако се изпълнява 0 пъти
    (lambda(x) x) ;тогава връщаме х
    (lambda (x)
      (f ((repeat f (- n 1)) x))))) ;иначе правим рекурсия