#lang htdp/bsl

; An N is one of:
; – 0
; – (add1 N)
; interpretation represents the counting numbers

; N -> Number
; computes (+ n pi) without using +
(check-within (add-to-pi 3) (+ 3 pi) 0.001)
(define (add-to-pi n)
  (cond [(zero? n) pi]
        [(positive? n) (add1 (add-to-pi (sub1 n)))]))

; N Number -> Number
; adds a natural number n to an arbitrary number x without using +
(check-expect (add 3 1) 4)
(check-expect (add 3 (- 1)) 2)
(check-expect (add 3 0.1) 3.1)
(check-expect (add 3 (- 0.1)) 2.9)
(check-within (add 3 pi) (+ 3 pi) 0.001)
(define (add n x)
  (cond [(zero? n) x]
        [(positive? n) (add1 (add (sub1 n) x))]))
