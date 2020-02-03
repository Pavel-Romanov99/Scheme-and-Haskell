;Списъци
(define l '(1 21 3 42 5 17 8))
(define lst '(67 17 8 29 10 21 3))
;1.Максимален елемент на списък
(define (maxElement l)
  (if(null? (cdr l))
     (car l)
     (max (car l) (maxElement (cdr l)))))

;2.Selection sort
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

;3.Взимаш елементите от позиция а до б
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

;5.Прави наредени двойки от съответните елементи на списъци
(define (zip l1 l2)
  (define (for l1 l2 result)
    (cond
      ((null? l1) result)
      (else (for (cdr l1) (cdr l2) (cons (cons (car l1) (car l2)) result)))))
  (for l1 l2 '()))

;6.Прилага функцията ф върху елементите на два списъка
(define (zip-with f l1 l2)
  (define (for f l1 l2 result)
    (cond
      ((null? l1) result)
      (else (for f (cdr l1) (cdr l2) (cons (f (car l1) (car l2)) result)))))
  (for f l1 l2 '()))

;7.Връща уникалните елементи на лист
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

;8.Сечение на два списъка
(define (intersection l1 l2)
  (define (for l1 l2 result)
    (cond
      ((or (null? l1) (null? l2)) result) ;ако и двата списъка са празни
      ((member? (car l1) l2) (for (cdr l1) l2 (cons (car l1) result))) ;ако имат общ елемент 
      (else (for (cdr l1) l2 result)))) ;иначе
  (for l1 l2 '()))

;9.Обединение на два списъка
(define (union l1 l2)
  (unique (append l1 l2)))

;10.Напишете функция, която връща списък от последователните подсписъци на L с дължина n
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