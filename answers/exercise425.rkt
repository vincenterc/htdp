#lang htdp/isl+

; [List-of Number] Number -> [List-of Number]
; filters out numbers that are larger than n from alon
(define (largers alon n)
  (cond
    [(empty? alon) '()]
    [else (if (> (first alon) n)
              (cons (first alon) (largers (rest alon) n))
              (largers (rest alon) n))]))

; [List-of Number] Number -> [List-of Number]
; filters out numbers that are smaller than n from alon
(define (smallers alon n)
  (cond
    [(empty? alon) '()]
    [else (if (< (first alon) n)
              (cons (first alon) (smallers (rest alon) n))
              (smallers (rest alon) n))]))
