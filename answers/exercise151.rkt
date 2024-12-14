#lang htdp/bsl

; An N is one of:
; – 0
; – (add1 N)
; interpretation represents the counting numbers

; N Number -> Number
; multiplies a natural number n with a number x without using *
(check-expect (multiply 0 2) 0)
(check-expect (multiply 3 2) 6)
(check-expect (multiply 3 0.1) 0.3)
(check-within (multiply 3 pi) (* 3 pi) 0.001)
(define (multiply n x)
  (cond [(zero? n) 0]
        [(positive? n) (+ x (multiply (sub1 n) x))]))

; Application
; (multiply 3 2)
