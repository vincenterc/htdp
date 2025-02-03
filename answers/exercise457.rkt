#lang htdp/isl+

; Number -> Number
; computes the months that is taken to double a mount of money for
; a given interest rate r
(check-expect (double-amount 1) 1)
(check-expect (double-amount 0.5) 2)
(check-expect (double-amount 0.3) 3)
(check-expect (double-amount 0.01) 70)
(define (double-amount r)
  (local (; Number -> Number
          ; computes the months that is taken to double base with
          ; the interest rate r
          (define (double-amount-aux base)
            (cond [(>= base 2) 0]
                  [else (add1 (double-amount-aux (* base (+ 1 r))))])))
    (double-amount-aux 1)))
