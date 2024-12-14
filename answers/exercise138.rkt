#lang htdp/bsl

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

; Application
; (sum (cons 20 (cons 10 '())))
