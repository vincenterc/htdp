#lang htdp/isl+

(define ε 0.01)
(define INTERVAL 0.1)

; [Number -> Number] Number Number -> Number
; computes the area under the graph of f between a and b
; assume (< a b) holds
(check-within (integrate-dc (lambda (x) 20) 12 22) 200 ε)
(check-within (integrate-dc (lambda (x) (* 2 x)) 0 10) 100 ε)
(check-within (integrate-dc (lambda (x) (* 3 (sqr x))) 0 10)
              1000
              ε)
(define (integrate-dc f a b)
  (cond [(<= (- b a) INTERVAL) (integrate-kepler f a b)]
        [else (local ((define mid (/ (+ a b) 2)))
                (+ (integrate-dc f a mid)
                   (integrate-dc f mid b)))]))

; [Number -> Number] Number Number -> Number
; computes the area under the graph of f between a and b
; assume (< a b) holds
(define (integrate-kepler f a b)
  (local ((define mid (/ (+ a b) 2))
          ; Number Number -> Number
          ; computes the area of the trapezoid formed by the
          ; four points (l, 0), (r, 0), (l, (f l)) and (r, (f r))
          (define (area-of-trapezoid l r)
            (/ (* (- r l) (+ (f l) (f r))) 2)))
    (+ (area-of-trapezoid a mid)
       (area-of-trapezoid mid b))))

; ε = 0.1  => INTERVAL = 0.3
; ε = 0.01 => INTERVAL = 0.1
