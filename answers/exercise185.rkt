#lang htdp/bsl+

(check-expect
 1
 (first (list 1 2 3)))

(check-expect
 (list 2 3)
 (rest (list 1 2 3)))

(check-expect
 2
 (second (list 1 2 3)))
