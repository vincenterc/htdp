#lang htdp/isl+

; [X Y] [X -> Y] [List-of X] -> [List-of Y]
; defines map using foldr
(check-expect
 (map add1 '(1 2 3))
 (map-from-fold add1 '(1 2 3)))
(define (map-from-fold f l)
  (local (; [X Y] [X] [List-of Y] -> [List-of Y]
          ; adds the result of the function on v
          ; at the beginning of the list acc
          (define (cons-with-f v acc)
            (cons (f v) acc)))
    (foldr cons-with-f '() l)))
