#lang htdp/bsl

; A List-of-temperatures is one of:
; – '()
; – (cons CTemperature List-of-temperatures)

(define ABSOLUTE0 -272)
; A CTemperature is a Number greater than ABSOLUTE0.

; List-of-temperatures -> Number
; computes the average temperature
(check-expect
 (average (cons 1 (cons 2 (cons 3 '())))) 2)
(define (average alot)
  (/ (sum alot) (how-many alot)))

; List-of-temperatures -> Number
; adds up the temperatures on the given list
(check-expect
 (sum (cons 1 (cons 2 (cons 3 '())))) 6)
(define (sum alot)
  (cond
    [(empty? alot) 0]
    [else (+ (first alot) (sum (rest alot)))]))

; List-of-temperatures -> Number
; counts the temperatures on the given list
(define (how-many alot)
  (cond
    [(empty? alot) 0]
    [else (+ (how-many (rest alot)) 1)]))

; List-of-temperatures -> Number
; computes the average temperature
(check-expect
 (checked-average (cons 1 (cons 2 (cons 3 '())))) 2)
(check-error (checked-average '()))
(define (checked-average alot)
  (cond [(not (empty? alot)) (average alot)]
        [else (error "average: a non-empty list expected")]))
