#lang htdp/isl+

(define JANUS
  (list 31.0
        #i2e+34
        #i-1.2345678901235e+80
        2749.0
        -2939234.0
        #i-2e+33
        #i3.2e+270
        17.0
        #i-2.4e+270
        #i4.2344294738446e+170
        1.0
        #i-8e+269
        0.0
        99.0))

; [List-of Number] -> Number
; computes the sum of
; the numbers on l
(define (sum l)
  (cond
    [(empty? l) 0]
    [else
     (+ (first l)
        (sum (rest l)))]))

; (sum JANUS)
; ==
; #i-1.2345678901235e+80

; (sum (reverse JANUS))
; ==
; #i99.0

; (sum (sort JANUS <))
; ==
; #i0.0

; (exact->inexact (sum (map inexact->exact JANUS)))
; ==
; #i4.2344294738446e+170
