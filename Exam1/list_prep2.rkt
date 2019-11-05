;sort digits
(define (getDigits n)
  (if(= n 0)
     '()
     (cons (remainder n 10) (getDigits(quotient n 10)))))

(define ra-sort
  (lambda (numList)
    (cond
      ((null? numList) '())
      ((= (car numList) (apply max numList))
          (cons (car numList) (ra-sort (cdr numList))))
          (else (ra-sort (append (cdr numList) (list (car numList))))
      ))))

(define a '(1 2 3 4 5))

;get the last element of the list
(define (last* lst)
  (if(null? (cdr lst))
     (car lst)
     (last* (cdr lst))))

;remove the last element of the list
(define (remove_last* lst)
  (if(null? (cdr lst))
     '()
     (cons (car lst) (remove_last* (cdr lst)))))

;reverse the list
(define (reverse* lst)
  (if(null? lst)
     '()
     (cons (last* lst) (reverse* (remove_last* lst)))))

;foldl
(define (foldl* op acc lst)
  (if(null? lst)
     acc
     (op (foldl* op acc (cdr lst)) (car lst))))

;foldr
(define (foldr* op acc lst)
  (if(null? lst)
     acc
     (op (car lst) (foldr* op acc (cdr lst)))))

;take the first n elements of a list
(define (remove** lst)
  (if(null? (cdr lst))
     '()
     (cons (car lst) (remove** (cdr lst)))))

(define (take lst n)
  (define (for counter lst)
    (if(= counter n)
       lst
       (for (+ counter 1) (remove** lst))))
  (for 0 lst))

;take the last n-elements of a list ??????????
(define removeFirst
  (lambda (input)
    (cond
      ((list? (car input)) (cons (removeFirst (car input)) (cdr input)))
      (else (cdr input))
    )
  )
)

(define (drop lst n)
  (define (for lst counter)
    (if(= counter n)
       lst
       (for (removeFirst lst) (+ counter 1))))
  (for lst 0))

;map
(define (map* lst f)
  (if(null? lst)
     '()
    (cons (f (car lst)) (map* (cdr lst) f))))

(define (1+ x)
  (+ x 1))

;filter
(define (filter* p? lst)
  (cond
    ((null? lst) '())
    ((p? (car lst)) (cons (car lst) (filter* p? (cdr lst))))
    (else (filter* p? (cdr lst)))))

;return the max element of a list
(define (max_el lst)
  (if(null? (cdr lst))
     (car lst)
     (if (< (car lst) (max_el (cdr lst)))  
             (max_el (cdr lst)) 
             (car lst)
         )))

;return the min element of a list
(define (min_el lst)
  (if(null? (cdr lst))
     (car lst)
     (if(> (car lst) (min_el (cdr lst)))
        (min_el (cdr lst))
        (car lst))))
