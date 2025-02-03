#lang htdp/isl+

(require 2htdp/abstraction)

(define ε 0.01)
(define R 160)

; [Number -> Number] Number Number -> Number
; computes the area under the graph of f between a and b
; assume (< a b) holds
(check-within (integrate-rectangles (lambda (x) 20) 12 22) 200 ε)
(check-within (integrate-rectangles (lambda (x) (* 2 x)) 0 10) 100 ε)
(check-within (integrate-rectangles (lambda (x) (* 3 (sqr x))) 0 10)
              1000
              ε)
(define (integrate-rectangles f a b)
  (local (; the width of each rectangle
          (define W (/ (- b a) R))
          (define first-mid (+ a (/ W 2)))
          ; N -> Number
          ; computes the area of the ith rectangle
          (define (area-of-rectangle i)
            (* W (f (+ first-mid (* i W))))))
    (for/sum ([i R]) (area-of-rectangle i))))

; ε = 0.1  => R = 50
; ε = 0.01 => R = 160
