#lang htdp/bsl

; A Price falls into one of three intervals:
; — 0 through 1000
; — 1000 through 10000
; — 10000 and above.
; interpretation the price of an item

(define FIRST-THRESHOLD 1000)
(define SECOND-THRESHOLD 10000)

(define FIST-TAX-RATE 0)
(define SECOND-TAX-RATE 0.05)
(define THIRD-TAX-RATE 0.08)

; Price -> Number
; computes the amount of tax charged for p
(check-expect (sales-tax 0) (* FIST-TAX-RATE 0))
(check-expect (sales-tax 537) (* FIST-TAX-RATE 537))
(check-expect (sales-tax 1000) (* SECOND-TAX-RATE 1000))
(check-expect (sales-tax 1282) (* SECOND-TAX-RATE 1282))
(check-expect (sales-tax 10000) (* THIRD-TAX-RATE 10000))
(check-expect (sales-tax 12017) (* THIRD-TAX-RATE 12017))
(define (sales-tax p)
  (cond
    [(and (<= 0 p) (< p FIRST-THRESHOLD)) (* FIST-TAX-RATE p)]
    [(and (<= FIRST-THRESHOLD p) (< p SECOND-THRESHOLD)) (* SECOND-TAX-RATE p)]
    [(>= p SECOND-THRESHOLD) (* THIRD-TAX-RATE p)]))
