#lang htdp/isl+

; Integer -> Integer
; determines an integer n such that (expt #i10. (+ n 1)) is #inf.0,
; while (expt #i10. n) is not, given an initial value for n
(define (n-to-inf n)
  (local ((define n+1 (add1 n)))
    (if (= (expt #i10. n+1) +inf.0)
        n
        (n-to-inf n+1))))

; (n-to-inf 0)
; ==
; 308
