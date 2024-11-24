#lang htdp/bsl

(define (show x)
  (cond
    [(and (string? x)
          (string=? "resting" x)) ...]
    [(<= -3 x -1) ...]
    [(>= x 0) ...]))
