#lang htdp/bsl

; A List-of-numbers is one of:
; – '()
; – (cons Number List-of-numbers)

; List-of-numbers -> List-of-numbers
; converts a list of US$ amounts into a list of € amounts
(check-expect (convert-euro '())
              '())
(check-expect (convert-euro (cons 1 '()))
              (cons 0.95 '()))
(check-expect (convert-euro (cons 5 (cons 1 '())))
              (cons 4.75 (cons 0.95 '())))
(define (convert-euro l)
  (cond
    [(empty? l) '()]
    [else (cons (* (first l) 0.95) (convert-euro (rest l)))]))

; Number List-of-numbers -> List-of-numbers
; converts a list of US$ amounts into a list of € amounts
; according to an exchange rate er
(check-expect (convert-euro* 0.95 '())
              '())
(check-expect (convert-euro* 0.95 (cons 1 '()))
              (cons 0.95 '()))
(check-expect (convert-euro* 0.95 (cons 5 (cons 1 '())))
              (cons 4.75 (cons 0.95 '())))
(define (convert-euro* er l)
  (cond
    [(empty? l) '()]
    [else (cons (* (first l) er) (convert-euro* er (rest l)))]))