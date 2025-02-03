#lang htdp/isl+

(define ε 0.01)
(define eps 0.2)

; [Number -> Number] Number Number -> Number
; computes the area under the graph of f between a and b
; assume (< a b) holds
(check-within (integrate-adaptive (lambda (x) 20) 12 22) 200 ε)
(check-within (integrate-adaptive (lambda (x) (* 2 x)) 0 10) 100 ε)
(check-within (integrate-adaptive (lambda (x) (* 3 (sqr x))) 0 10)
              1000
              ε)
(define (integrate-adaptive f a b)
  (local ((define mid (/ (+ a b) 2))
          ; Number Number -> Number
          ; computes the area of the trapezoid formed by the
          ; four points (l, 0), (r, 0), (l, (f l)) and (r, (f r))
          (define (area-of-trapezoid l r)
            (/ (* (- r l) (+ (f l) (f r))) 2))
          (define trapezoid1 (area-of-trapezoid a mid))
          (define trapezoid2 (area-of-trapezoid mid b)))
    (cond [(<= (abs (- trapezoid1 trapezoid2))
               (* eps (- b a)))
           (+ trapezoid1 trapezoid2)]
          [else
           (+ (integrate-adaptive f a mid)
              (integrate-adaptive f mid b))])))

; ε = 0.1  => eps = 1
; ε = 0.01 => eps = 0.2
