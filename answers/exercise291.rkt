#lang htdp/isl+

; [X Y] [X -> Y] [List-of X] -> [List-of Y]
; defines map using foldr
(check-expect
 (map add1 '(1 2 3))
 (map-from-fold add1 '(1 2 3)))
(define (map-from-fold f l)
  (foldr (lambda (v acc) (cons (f v) acc)) '() l))
