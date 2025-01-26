#lang htdp/isl+

; [List-of Number] [List-of Number] -> Number
; produces the value of an linear combination lc
; according to a list of variable values lovv
; assumes lc and lovv have the same length
(check-expect (value '() '()) 0)
(check-expect (value '(5) '(10)) 50)
(check-expect (value '(5 17) '(10 1)) 67)
(check-expect (value '(5 17 3) '(10 1 2)) 73)
(define (value lc lovv)
  (cond
    [(empty? lc) 0]
    [else (+ (* (first lc) (first lovv))
             (value (rest lc) (rest lovv)))]))
