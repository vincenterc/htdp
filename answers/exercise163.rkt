#lang htdp/bsl

; Number -> Number
; converts Fahrenheit temperatures to Celsius temperatures
(check-expect (f2c -40) -40)
(check-expect (f2c 32) 0)
(check-expect (f2c 212) 100)
(define (f2c f)
  (* 5/9 (- f 32)))

; A List-of-numbers is one of:
; – '()
; – (cons Number List-of-numbers)

; List-of-numbers -> List-of-numbers
; converts Fahrenheit temperatures to Celsius temperatures
(check-expect (convertFC '())
              '())
(check-expect (convertFC (cons -40 '()))
              (cons -40 '()))
(check-expect (convertFC (cons -40 (cons 32 '())))
              (cons -40 (cons 0 '())))
(check-expect (convertFC (cons -40 (cons 32 (cons 212 '()))))
              (cons -40 (cons 0 (cons 100 '()))))
(define (convertFC l)
  (cond [(empty? l) '()]
        [else (cons (f2c (first l))
                    (convertFC (rest l)))]))
