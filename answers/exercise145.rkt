#lang htdp/bsl

; An NEList-of-temperatures is one of:
; – (cons CTemperature '())
; – (cons CTemperature NEList-of-temperatures)
; interpretation non-empty lists of Celsius temperatures

(define ABSOLUTE0 -272)
; A CTemperature is a Number greater than ABSOLUTE0.

; NEList-of-temperatures -> Boolean
; determines if the temperatures in ne-l are sorted in descending order
(check-expect (sorted>? (cons 2 '())) #true)
(check-expect (sorted>? (cons 1 (cons 2 '()))) #false)
(check-expect (sorted>? (cons 3 (cons 2 '()))) #true)
(check-expect (sorted>? (cons 0 (cons 3 (cons 2 '())))) #false)
(define (sorted>? ne-l)
  (cond [(empty? (rest ne-l)) #true]
        [else (and (>= (first ne-l) (first (rest ne-l)))
                   (sorted>? (rest ne-l)))]))