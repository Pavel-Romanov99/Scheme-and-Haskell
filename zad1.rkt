
(define (digit-function digit)
  (lambda args
    (cond
      ((null? args)           digit)
      ((null? (cdr args))     ((car args) digit))
      (else                   null))))

(define (operation-function operation)
  (lambda (rightArg) (lambda (leftArg) (operation leftArg rightArg))))

(define zero    (digit-function 0))
(define one     (digit-function 1))
(define two     (digit-function 2))
(define three   (digit-function 3))
(define four    (digit-function 4))
(define five    (digit-function 5))
(define six     (digit-function 6))
(define seven   (digit-function 7))
(define eight   (digit-function 8))
(define nine    (digit-function 9))
 
(define plus    (operation-function +))
(define minus   (operation-function -))
(define times   (operation-function *))
(define div     (operation-function /))


