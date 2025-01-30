#lang htdp/isl+

; Number N -> Number
; computes the power of n to p
(check-expect (my-expt 2 3) 8)
(check-expect (my-expt 3 2) 9)
(define (my-expt n p)
  (cond [(zero? p) 1]
        [else (* n (my-expt n (sub1 p)))]))

(define inex (+ 1 #i1e-12))
(define exac (+ 1 1e-12))

; (my-expt inex 30)
; ==
; #i1.0000000000300027

; (my-expt exac 30)
; ==
; 1.0000000000300000000004350...
