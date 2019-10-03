;намира номера на подаден месец от годината;
(define (month x)(cond
                   ((equal? x "january") (display "1"))
                   ((equal? x "febuary") (display "2"))
                   ((equal? x "march") (display "3"))
                   ((equal? x "april") (display "4"))
                   ((equal? x "may") (display "5"))
                   ((equal? x "june") (display "6"))
                   ((equal? x "july") (display "7"))
                   ((equal? x "august") (display "8"))
                   ((equal? x "september") (display "9"))
                   ((equal? x "october") (display "10"))
                   ((equal? x "november") (display "11"))
                   ((equal? x "december") (display "12"))
                   ))
                   
;проверява дали х е корен на квадратното уравнение;
(define (is-root? x)(if
                     (zero? (- (- (* 3  (* x x)) (* 2 x)) 1)) #t
                     #f
                     ))

;намира n!;
(define (factoriel x)
  (cond ((< x 0) #f)
  ((<= x 1) 1)
(else (* x (factoriel (- x 1))))))

;fibonacci;
(define (fibonacci x)
  (cond ((equal? x  1) 1)
        ((equal? x  2) 1)
        (else (+ (fibonacci (- x 1)) (fibonacci (- x 2))))))

;за дадено число х връща х/2 ако х е четно, и х в противен случай;
(define (safe-div x)
  (if (zero? (remainder  x 2))
      (/ x 2)
      x))

;за дадено ест число х намира х+1;
(define (succ x)
  (+ x 1))

;за дадено ест число х намира х-1;
(define (pred x)
  (- x 1))
