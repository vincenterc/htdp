#lang htdp/isl+

; [X -> Y] [List-of X] -> [List-of Y]
; constructs a list by applying f to each item on lx0
(check-expect (map* add1 '(1 2 3)) '(2 3 4))
(check-expect (map* odd? '(1 2 3 4)) '(#true #false #true #false))
(define (map* f lx0)
  (local (; [List-of X] [List-of Y] -> [List-of Y]
          ; constructs a list by applying f to each item on lx
          ; accumulator a is a list of items that lx lacks
          ; from lx0, applied by f in reverse order
          (define (map*/a lx a)
            (cond
              [(empty? lx) (reverse a)]
              [else
               (map*/a (rest lx) (cons (f (first lx)) a))])))
    (map*/a lx0 '())))
