#lang htdp/isl+

(define EPS 0.0001)

; [Number -> Number] Number -> Number
; computes the slope of f at r1
(check-expect (slope (lambda (x) 1) 1) 0)
(check-expect (slope (lambda (x) x) 1) 1)
(check-expect (slope (lambda (x) (* x x)) 1) 2)
(define (slope f r1)
  (/ (- (f (+ r1 EPS)) (f (- r1 EPS))) (* 2 EPS)))
