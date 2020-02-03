;Упражнения за листи
(define l (list 1 2 3 4 5 6 7 8 13 14 5))

;1.List lenght
(define (length* lst)
  (if(null? lst)
     0
     (+ (length* (cdr lst)) 1)))

;2.List sum
(define (sum* lst)
  (if(null? lst)
     0
     (+ (sum* (cdr lst)) (car lst))))

;3.Return last element of a list
(define (last lst)
  (if(null? (cdr lst))
     (car lst)
     (last (cdr lst))))

;4.Append two lists
(define (append* lst1 lst2)
  (if(null? lst1)
     lst2
     (cons (car lst1) (append* (cdr lst1) lst2))))

;5.Push back
(define (push-back x lst)
  (if(null? lst)
     (list x)
     (cons (car lst) (push-back x (cdr lst)))))

;6.Check if x is a member of the list
(define (member? x lst)
  (if(null? lst)
     #f
     (if(= (car lst) x)
        #t
        (member? x (cdr lst)))))

;7.Create a list from an interval [a,b]
(define (from-to a b)
  (if(= a b)
     (list b)
     (cons a (from-to (+ a 1) b))))

;8.Reverse a list
(define (reverse* lst)
  (if(null? lst)
     lst
     (push-back (car lst) (reverse* (cdr lst)))))

;9.Map
(define (map* f lst)
  (if(null? lst)
     lst
     (cons (f (car lst)) (map* f (cdr lst)))))

;10.Filter
(define (filter? p? lst)
  (cond
    ((null? lst) lst)
    ((p? (car lst)) (cons (car lst) (filter? p? (cdr lst))))
    (else (filter? p? (cdr lst)))))

;11.Връща списък от два списъка - един на който е верен предиката, и друг с грешен предикат
(define (partition p? lst)
  (define (for p? lst lst1 lst2)
    (cond
      ((null? lst) (list lst1 lst2))
      ((p? (car lst)) (for p? (cdr lst) (cons (car lst) lst1) lst2))
      (else (for p? (cdr lst) lst1 (cons (car lst) lst2)))))
  (for p? lst '() '()))

;12.Сумата на третите степени на всички прости числа в списък
(define (div-sum n)
  (define (for n counter sum)
    (cond
      ((= n counter) (+ sum counter))
      ((zero? (remainder n counter)) (for n (+ counter 1) (+ sum counter)))
      (else (for n (+ counter 1) sum))))
  (for n 1 0))

(define (prime? n)
  (if(= (div-sum n) (+ 1 n))
     #t
     #f))

(define (scp lst)
  (define (for lst sum)
    (cond
      ((null? lst) sum)
    ((prime? (car lst))
       (for (cdr lst) (+ sum (* (* (car lst) (car lst)) (car lst)))))
      (else (for (cdr lst) sum))))
  (for lst 0))

;13.Връща първите n елемента на списък
(define (take n lst)
  (define (for n lst new_lst)
    (if(zero? n)
       new_lst
       (for (- n 1) (cdr lst) (push-back (car lst) new_lst))))
  (for n lst '()))

;14.Връща списък от всички елементи без първите n
(define removeFirst
  (lambda (input)
    (cond
      ((list? (car input)) (cons (removeFirst (car input)) (cdr input)))
      (else (cdr input)))))

(define (drop n lst)
  (if(zero? n)
     lst
     (drop (- n 1) (removeFirst lst))))

;15.Връща позиция на х в листа
(define (position x lst)
  (define (for x lst counter)
    (cond
      ((null? lst) -1)
      ((= x (car lst)) counter)
      (else (for x (cdr lst) (+ counter 1)))))
  (for x lst 0))

;16.Връща всички елементи на позиция > х
(define (list-tail* lst n)
  (cond
    ((null? lst) lst)
    ((= n 0) (cdr lst))
    (else (list-tail (cdr lst) (- n 1)))))

;17.Insert x at position n in list lst
(define (insert* x n lst)
  (cond
    ((null? lst) (list x)) ;ако листа е празен се вмъква само х 
    ((= n 0) (cons x lst)) ;ако сложим на позиция 0, то слагаме х в началото на списъка
    (else (cons (car lst) (insert* x (- n 1) (cdr lst)))))) ;слагаме елементи докато н стане 0,тогава
;се добавя х в края и продължаваме да смаляваме листа до достигане на дъното на рекурсията

;18.Премахва първото срещане на х в списъка
(define (remove* x lst)
  (cond
    ((null? lst) lst)
    ((= x (car lst)) (cdr lst))
    (else (cons (car lst) (remove* x (cdr lst))))))

;19.Прави списък от цифрите на дадено число
(define (explode-digits n)
    (cond
      ((< n 10) (list n))
      ((= n 0) '())
      (else (cons (remainder n 10) (explode-digits (quotient n 10))))))

;20.Koлко пъти цифрата х се среща в дадено число
(define (digit-occurance x n)
  (define (for lst x counter)
    (cond
      ((null? lst) counter)
      ((= x (car lst)) (for (cdr lst) x (+ counter 1)))
      (else (for (cdr lst) x counter))))
  (for (explode-digits n) x 0))

;21.Премахва последователните повторения на едно и също число в списък - hard
(define (remove-from-beginning x L)
  (cond ((null? L) L)
        ((equal? x (car L)) (remove-from-beginning x (cdr L)))
        (else L)))
      
(define (remove-repeats L)
  (if (null? L)
      L
      (cons (car L)
            (remove-repeats (remove-from-beginning (car L) L)))))

;Списъци
(define l '(1 21 3 42 5 17 8))
(define lst '(67 17 8 29 10 21 3))
;22.Максимален елемент на списък
(define (maxElement l)
  (if(null? (cdr l))
     (car l)
     (max (car l) (maxElement (cdr l)))))

;23.Selection sort
(define (minElement l)
  (if(null? (cdr l))
     (car l)
     (min (car l) (minElement (cdr l)))))

(define (removeElement x l)
  (cond
    ((null? l) '())
    ((= x (car l)) (cdr l))
    (else (cons (car l) (removeElement x (cdr l))))))

(define (reverse* l)
  (cond
    ((null? (cdr l)) (list(car l)))
    (else (append (reverse* (cdr l)) (list (car l))))))

(define (selectionSort l)
  (define (for l result)
    (cond
      ((null? l) (reverse* result))
      (else (for (removeElement (minElement l) l) (cons (minElement l) result)))))
  (for l '()))

;24.Взимаш елементите от позиция а до б
(define (removeFirst l)
  (if(null? l)
     '()
     (cdr l)))

;removes the first n elements of a list
(define (drop* n l)
  (cond
    ((null? l) '())
    ((= n 0) l)
    (else (drop* (- n 1) (removeFirst l)))))

(define (removeLast l)
  (if(null? (cdr l)) ;if its the last element
     '() ;  връщаме празен списък
     (cons (car l) (removeLast (cdr l)))));иначе добавяме ел до достигане на последния ел.

;takes the first n elements of a list
(define (take* n l)
  (cond
    ((null? l) '())
    ((= n 0) l)
    (else (take* (- n 1) (removeLast l)))))

(define (split a b l)
  (take* (+ 1 (- b a)) (drop* a l)))

;25.Прави наредени двойки от съответните елементи на списъци
(define (zip l1 l2)
  (define (for l1 l2 result)
    (cond
      ((null? l1) result)
      (else (for (cdr l1) (cdr l2) (cons (cons (car l1) (car l2)) result)))))
  (for l1 l2 '()))

;26.Прилага функцията ф върху елементите на два списъка
(define (zip-with f l1 l2)
  (define (for f l1 l2 result)
    (cond
      ((null? l1) result)
      (else (for f (cdr l1) (cdr l2) (cons (f (car l1) (car l2)) result)))))
  (for f l1 l2 '()))

;27.Връща уникалните елементи на лист
(define (member? x lst)
  (cond
    ((null? lst) #f)
    ((= x (car lst)) #t)
    (else (member? x (cdr lst)))))

(define (unique l)
  (define (for l result)
    (cond
      ((null? l) (reverse* result))
      ((not(member? (car l) result)) (for (cdr l) (cons (car l) result))) ;ако е уникален отива в резултата
      (else (for (cdr l) result)))) ;иначе продължаваме да гледаме напред
  (for l '()))

;28.Сечение на два списъка
(define (intersection l1 l2)
  (define (for l1 l2 result)
    (cond
      ((or (null? l1) (null? l2)) result) ;ако и двата списъка са празни
      ((member? (car l1) l2) (for (cdr l1) l2 (cons (car l1) result))) ;ако имат общ елемент 
      (else (for (cdr l1) l2 result)))) ;иначе
  (for l1 l2 '()))

;29.Обединение на два списъка
(define (union l1 l2)
  (unique (append l1 l2)))

;30.Напишете функция, която връща списък от последователните подсписъци на L с дължина n
(define (get_first_n_elements n lst)
  (cond
    ((null? lst) '())
    ((= n 0) '())
    (else (cons (car lst) (get_first_n_elements (- n 1) (cdr lst))))))

(define (chunk n l)
  (define (for n l result)
    (cond
      ((null? l) result)
      (else (for n (drop* n l) (cons (get_first_n_elements n l) result)))))
  (for n l '()))

;31.Поставя елемент на правилното място в сортиран списък
(define (insert x lst)
  (cond
    ((null? lst) (list x))
    ((< x (car lst)) (cons x lst))
    (else (cons (car lst) (insert x (cdr lst))))))

;32.Отворен числов интервал (a;b) се описва с наредената двойка (a . b).
;Да се напише функция longest-interval-subsets, която по даден списък от интервали
;il връща нов списък, който съдържа всички интервали от il, които са подинтервали
;на най-дългия интервал в списъка. Бонус: Функцията longest-interval-subsets да връща
;подинтервалите подредени в нарастващ ред по началната си точка.

(define (sub? i1 i2) ; дали i1 е подинтервал на i2
  (and (>= (car i1) (car i2))
       (<= (cdr i1) (cdr i2))))

(define (max-interval lst) ; максимален по дължина интервал
  (define (int-length i) ; дължина на интервал
    (- (cdr i) (car i)))
  (define (for currMax lst)
    (cond ((null? lst) currMax) ;ако листа е празен
          ((> (int-length (car lst)) (int-length currMax)) ;ако текущ елемент е по-голям от макс ел
             (for (car lst) (cdr lst))) ;тогава макс ел = текущ ел и продължаваме да търсим
          (else
             (for currMax (cdr lst))))) ;иначе продължаваме да търсим
  ; При търсене на мин/макс взимаме първия елемент
  ; като първоначален и обхождаме останалите.
  (for (car lst) (cdr lst)))

(define (longest-interval-subsets lst)
  (define longest (max-interval lst)) ; за да не го преизчисляваме непрекъснато
  (filter? (lambda (i) (sub? i longest)) lst))