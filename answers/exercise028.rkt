#lang htdp/bsl

(define (profit price)
  (- (* (+ 120
           (* (/ 15 0.1)
              (- 5.0 price)))
        price)
     (+ 180
        (* 0.04
           (+ 120
              (* (/ 15 0.1)
                 (- 5.0 price)))))))

(profit 1)   ; 511.2
(profit 2)   ; 937.2
(profit 2.5) ; 1037.7
(profit 2.7) ; 1056.9
(profit 2.8) ; 1062
(profit 2.9) ; 1064.1
(profit 3)   ; 1063.2
(profit 3.1) ; 1059.3
(profit 3.2) ; 1052.4
(profit 3.5) ; 1013.7
(profit 4)   ; 889.2
(profit 5)   ; 415.2