;Структури данни в scheme
(define (all? p? l)
  (foldr (lambda (x y) (and x y)) #t (map p? l)))


(define (matrix? m) ;проверка за матрица
  (and (list? m)
       (not (null? (car m)))
       (all? list? m)
       (all? (lambda (row) (= (length row)
                              (length (car m)))) m)))

;Брой редове и стълбове
(define get-rows length)
(define (get-columns m) (length (car m)))

;Намиране на първи ред и стълб
(define get-first-row car)
(define (get-first-column m) (map car m))

;Изтриване на първи ред и стълб
(define del-first-row cdr)
(define (del-first-column m) (map cdr m))

;Намиране на ред и стълб по индекс
(define (get-row i m) (list-ref m i))

(define (get-column i m)
  (map (lambda (row) (list-ref row i)) m))

;1.Транспониране на матрица
(define (transpose matrix)
  (if(null? (get-first-row matrix))
     '()
     (cons (get-first-column matrix)
           (del-first-column matrix))))

;2.Събиране на матрици
(define (sum-vectors v1 v2) (map + v1 v2))
(define (sum-matrices m1 m2) (map sum-vectors m1 m2))

;3.Умножение на матрици
(define (mult-vectors v1 v2) (apply + (map * v1 v2)))

(define (mult-matrices m1 m2)
  (let ((m2t (transpose m2)))
    (map (lambda (row)
           (map (lambda (column) (mult-vectors row column))
                m2t))
         m1)))



;Работа с дървета
(define (tree? t) ;проверка за дърво
  (or (null? t)
      (and (list? t)
           (= (length t) 3))
           (tree? (cadr t))
           (tree? (caddr t))))


(define empty-tree '()) ;създаване на празно дърво

(define (make-tree root left right) (list root left right))      ; не искаме просто (define make-tree list) - защо?

(define (make-leaf root) (make-tree root empty-tree empty-tree)); за удобство

(define root-tree car) ;вземане на корена на дърво

(define left-tree cadr) ;вземане на ляво поддърво

(define right-tree caddr) ;вземане на дъсно поддърво

(define empty-tree? null?) ;проверка дали едно дърво е празно

(define t
  (make-tree 5
             (make-tree 4
                        (make-leaf 3)
                        empty-tree)
             (make-tree 8
                        (make-tree 7
                                   (make-leaf 10)
                                   empty-tree)
                        (make-leaf 9))))

;1.Сумата на всички елементи на дърво
(define (tree-sum t)
  (cond
    ((empty-tree? t) 0)
    (else (+ (root-tree t) (+ (tree-sum (left-tree t)) (tree-sum (right-tree t)))))))

;2.Връща всички елементи на височина к
(define (tree-level k t)
  (define (for k t result)
    (cond
      ((empty-tree? t) result)
      ((= k 0) (cons (root-tree t) result))
      (else (append (for (- k 1) (left-tree t) result) (for (- k 1) (right-tree t) result)))))
  (for k t '()))

;3.Връща списък от всички нива на дърво започвайки от нулевото надолу
(define (all-levels t)
  (cond
    ((empty-tree? t) '())
    (else (append (list (root-tree t)) (all-levels (right-tree t)) (all-levels (left-tree t))))))

;4.Tree map
(define (1+ x)
  (+ x 1))

(define (tree-map f t)
  (cond
    ((empty-tree? t) '())
    (else (list (f (root-tree t)) (tree-map f (left-tree t)) (tree-map f (right-tree t))))))

;5.Обхождане ляво-корен-дясно
(define (inorder t)
  (cond
    ((empty-tree? t) empty-tree)
    (else (append (inorder (left-tree t)) (list (root-tree t)) (inorder (right-tree t))))))

;6.Insert in a binary search tree
  (define (insert el tree)
  (cond ((null? tree) 
         (list el '() '()))
        ((= el (root-tree tree))
         tree)
        ((< el (root-tree tree))
         (list (root-tree tree)  
               (insert el (left-tree tree))
               (right-tree tree)))
        ((> el (root-tree tree))
         (list (root-tree tree)
               (left-tree tree)
               (insert el (right-tree tree))))))

;7.Sort a tree using bst and inorder traversal
(define (tree-sort lst)
  (define (for lst t)
    (cond
      ((null? lst) (inorder t))
      (else (for (cdr lst) (insert (car lst) t)))))
  (for lst empty-tree))

;8.Check if a tree is a valid bst
(define (valid-bst? t)
  (define (for t lst)
    (cond
      ((<= (length lst) 1) #t)
      (( and (> (car lst) (car (cdr lst))) (not (null? (car (cdr lst))))) #f)
      (else (for t (cdr lst)))))
  (for t (inorder t)))

;9.Премахва всички листа на дърво
(define (prune t)
  (cond
    ((empty-tree? t) '())
    ((and (empty-tree? (left-tree t)) (empty-tree? (right-tree t))) '())
    (else (list (root-tree t) (prune (left-tree t)) (prune (right-tree t))))))

;10.Заменя всяко листо със стойност х с дърво с корен х и ляво-дясно дърво с корен х
(define (bloom t)
  (cond
    ((empty-tree? t) empty-tree)
    ((and (empty-tree? (left-tree t)) (empty-tree? (right-tree t)))
     (make-tree
      (root-tree t)
      (make-leaf (root-tree t))
      (make-leaf (root-tree t))))
    (else (make-tree
           (root-tree t)
           (bloom (left-tree t))
           (bloom (right-tree t))))))

;11.Tree height
(define (height t)
  (cond
    ((empty-tree? t) 0)
    (else (max (+ (height (left-tree t)) 1) (+ (height (right-tree t)) 1)))))

;12.Invert a tree
(define (invert t)
  (cond
    ((empty-tree? t) (list empty-tree))
    (else (make-tree
           (root-tree t)
           (make-leaf (right-tree t))
           (make-leaf (left-tree t))))))

;Асоциативни списъци
(define (square x)
  (* x x))

;Създаване на списъка
(define (make-hash f keys)
  (map (lambda(x) (cons x (f x))) keys))

(define lst(make-hash square '(1 1 1 3 5 7 6)))

;вземане на ключовете
(define (get-keys hash)
  (map car hash))

;вземане на стойностите
(define (get-value hash)
  (map cdr hash))

;1.Връщане на двойка ключ и стойност по даден ключ
(define (assoc* x hash)
  (cond
    ((null? hash) #f) ;ако е празен 
    ((= x (caar hash)) (car hash)) ;ако х е равен на ключа връщаме наредената двойка
    (else (assoc* x (cdr hash))))) ;иначе продължаваме да търсим

;2.Изтриване на елемент по даден ключ
(define (del-el x hash)
  (define (for x hash result)
    (cond
      ((null? hash) result) ;ако е празен
      ((= x (caar hash)) (for x (cdr hash) result)) ;ако ключът е равен на х, тогава махаме елемента от хеша
      (else (for x (cdr hash) (cons (car hash) result))))) ;иначе продължаваме да търсим
  (for x hash '()))

;3.Задаване на стойност на ключ. Изтрива се стария ключ ако има такъв
(define (insert-el key value hash)
  (define (for key value hash result)
    (cond
      ((null? hash) result)
      ;ако ключа е равен търсения, тогава трием елемента и го заместваме с новия 
      ((= key (caar hash)) (for key value (del-el (caar hash) hash) (cons (cons key value) result)))
      ;иначе продължаваме да търсим
      (else (for key value (cdr hash) (cons (car hash) result)))))
  (for key value hash '()))

;4.Да се намери има ли елемент на хеша, който отговаря на предикат
(define (search p? hash)
  (cond
    ((null? hash) #f)
    ((p? (caar hash)) (car hash))
    (else (search p? (cdr hash)))))

;5.Map
(define (map-hash f hash)
  (cond
    ((null? hash) '())
    (else (cons (cons (f (caar hash)) (cdar hash)) (map-hash f (cdr hash))))))

;6.Filter
(define (filter-hash p? hash)
  (cond
    ((null? hash) '())
    ((p? (caar hash)) (cons (cons (caar hash) (cdar hash)) (filter-hash p? (cdr hash))))
    (else (filter-hash p? (cdr hash)))))

;7.Проверка дали всички наредени двойки отговарят на даден предикат
(define (all-hash? p? hash)
  (cond
    ((null? hash) #t)
    ((not(p? (caar hash))) #f)
    (else (all-hash? p? (cdr hash)))))

;8.Напишете функция (index l), която връща асоциативен списък,
;в който всеки елемент x на l е асоцииран с ключ, равен на позицията на x в l.
(define (index hash)
  (define (for hash counter result)
    (cond
      ((null? hash) result)
      ((= counter (caar hash)) (for (cdr hash) (+ counter 1) (cons (car hash) result)))
      (else (for (cdr hash) (+ counter 1) result))))
  (for hash 0 '()))

;9.Прави асоциативен списък от колко пъти дадено число се среща в даден списък

;how many times a number is found in a list
(define (count-number x lst)
  (define (for x lst counter)
    (cond
      ((null? lst) counter)
      ((= x (car lst)) (for x (cdr lst) (+ counter 1)))
      (else (for x (cdr lst) counter))))
  (for x lst 0))

;delete all occurances of a number in a list
(define (del-occurances x lst)
  (define (for x lst result)
    (cond
      ((null? lst) result)
      ((= x (car lst)) (for x (cdr lst) result))
      (else (for x (cdr lst) (cons (car lst) result)))))
  (for x lst '()))
      
(define (histogram lst)
  (define (for lst result)
    (cond
      ((null? lst) result)
      (else (for (del-occurances (car lst) lst) (cons (cons (car lst) (count-number (car lst) lst)) result))))) 
  (for lst '()))

;10.Напишете функция, която в асоциативен списък изчиства дублирани ключове, като запазва само първата стойност.