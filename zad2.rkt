
(define (list-apply-reverse list selector) (_list-apply-reverse list selector `()))
(define (_list-apply-reverse list selector result)
  (cond
    ((null? list)        result)
    (else                 (_list-apply-reverse (cdr list) selector (cons (selector (car list)) result)))))
 
(define (list-reverse list) (list-apply-reverse list (lambda (x) x)))
 
(define (prefixes xs) (list-apply-reverse (_prefixes xs `(())) list-reverse))
 
(define (_prefixes list prefixList)
  (cond
    ((null? list)     prefixList)
    (else              (_prefixes (cdr list) (cons (cons (car list) (car prefixList)) prefixList)))))