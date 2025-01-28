#lang htdp/isl+

; N -> Number
; adds up n copies of #i1/185
(check-expect (add 0) 0)
(check-within (add 1) #i1/185 0.0001)
(define (add n)
  (cond [(zero? n) 0]
        [else (+ #i1/185
                 (add (sub1 n)))]))

; Number -> N
; counts how often 1/185 can be subtracted from the argument
; until it is 0
(check-expect (sub 0) 0)
(check-expect (sub 1/185) 1)
(define (sub n)
  (cond [(zero? n) 0]
        [else (add1 (sub (- n 1/185)))]))

; Application
; (add 185)
; (sub 1)
; (sub #i1.0)
