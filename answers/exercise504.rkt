#lang htdp/isl+

; [List-of Number] -> Number
; produces the corresponding number of the list of digits lod0
(check-expect (to10 '(1 0 2)) 102)
(define (to10 lod0)
  (local (; [List-of Number] N Number -> Number
          ; produces the corresponding number of
          ; the list of digits lod
          ; accumulator a is the sum of numbers that
          ; lod lacks from lod0, multiplied by its corresponding
          ; power of 10 based on its position in lod
          (define (to10/a lod a)
            (cond
              [(empty? lod) a]
              [else (to10/a (rest lod) (+ (first lod) (* a 10)))])))
    (to10/a lod0 0)))
