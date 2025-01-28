#lang htdp/isl+

; Integer -> Integer
; determines an integer n such that (expt #i10. (+ n 1)) is #i0.0,
; while (expt #i10. n) is not, given an initial value for n
(define (n-to-#i0 n)
  (local ((define n-1 (sub1 n)))
    (if (= (expt #i10. n-1) #i0.0)
        n
        (n-to-#i0 n-1))))

; (n-to-#io 0)
; ==
; -323

; Integer -> Integer
; determines an integer n such that (expt #i10. (op m)) reaches an extreme
; while (expt #i10. n) does not
(check-expect (n-to-extreme 0 add1 +inf.0) 308)
(check-expect (n-to-extreme 0 sub1 #i0.0) -323)
(define (n-to-extreme n op extreme)
  (local ((define n1 (op n)))
    (if (= (expt #i10. n1) extreme)
        n
        (n-to-extreme n1 op extreme))))
