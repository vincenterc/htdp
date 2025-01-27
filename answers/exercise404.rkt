#lang htdp/isl+

; [X Y] [X Y -> Boolean] [List-of X] [List-of Y] -> Boolean
; determines whether f holds for every pair of corresponding values
; from lx and ly
; (andmap2 f (list x-1 ... x-n) (list y-1 ... y-n)) ==
; (and (f x-1 y-1) ... (f x-n y-n))
; assume lx and ly are of equal length
(check-expect (andmap2 < '() '()) #true)
(check-expect (andmap2 < '(1) '(2)) #true)
(check-expect (andmap2 < '(1 2) '(2 3)) #true)
(check-expect (andmap2 < '(1 2 3) '(2 3 4)) #true)
(check-expect (andmap2 < '(1 2 3) '(2 0 4)) #false)
(define (andmap2 f lx ly)
  (cond [(empty? lx) #true]
        [else (and (f (first lx) (first ly))
                   (andmap2 f (rest lx) (rest ly)))]))
