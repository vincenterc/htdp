#lang htdp/isl+

; N [>=1] -> Boolean
; determines whether n0 is a prime number
(check-expect (is-prime? 2) #true)
(check-expect (is-prime? 13) #true)
(check-expect (is-prime? 41) #true)
(check-expect (is-prime? 4) #false)
(check-expect (is-prime? 102) #false)
(define (is-prime? n0)
  (cond
    [(= n0 1) #false]
    [else
     (local (; N N -> N
             ; determines whether n is a prime number
             ; accumulator a is the initial number n0
             (define (is-prime?/a n a)
               (cond
                 [(= n 1) #true]
                 [(= (modulo a n) 0) #false]
                 [else (is-prime?/a (sub1 n) a)])))
       (is-prime?/a (sub1 n0) n0))]))
