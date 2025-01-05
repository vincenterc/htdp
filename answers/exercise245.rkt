#lang htdp/isl

(define (function=at-1.2-3-and-5.775? f1 f2)
  (and (= (f1 1.2) (f2 1.2))
       (= (f1 3) (f2 3))
       (= (f1 -5.775) (f2 -5.775))))

; Can not define function=? because you can not try all inputs,
; as there are not a finite number of them.
