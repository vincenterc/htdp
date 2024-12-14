#lang htdp/bsl

; A List-of-numbers is one of:
; – '()
; – (cons Number List-of-numbers)

; List-of-numbers -> Number
; determines whether all numbers in alon are positive numbers
(check-expect (pos? '()) #true)
(check-expect (pos? (cons 5 '())) #true)
(check-expect (pos? (cons -1 '())) #false)
(check-expect (pos? (cons 10 (cons 5 '()))) #true)
(check-expect (pos? (cons 5 (cons -1 '()))) #false)
(define (pos? alon)
  (cond
    [(empty? alon) #true]
    [else (and (> (first alon) 0)
               (pos? (rest alon)))]))

; Application
; (pos? (cons 5 '()))
; (pos? (cons -1 '()))

; A List-of-amounts is one of:
; – '()
; – (cons PositiveNumber List-of-amounts)

; List-of-amounts -> Number
; computes the sum of the amounts in aloa
(check-expect (sum '()) 0)
(check-expect (sum (cons 10 '())) 10)
(check-expect (sum (cons 20 (cons 10 '()))) 30)
(define (sum aloa)
  (cond
    [(empty? aloa) 0]
    [else (+ (first aloa)
             (sum (rest aloa)))]))

; List-of-numbers -> Number
; computes the sum of the numbers in alon
; if all numbers in alon are positive numbers
(check-expect (checked-sum '()) 0)
(check-expect (checked-sum (cons 10 '())) 10)
(check-expect (checked-sum (cons 20 (cons 10 '()))) 30)
(check-error (checked-sum (cons -10 '())))
(check-error (checked-sum (cons 20 (cons -10 '()))))
(define (checked-sum alon)
  (cond [(pos? alon) (sum alon)]
        [else (error "sum: a list of positive numbers expected")]))
