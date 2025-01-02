#lang htdp/isl

(define one-list '(("a" 1) ("b" 2) ("c" 3) ("d" 4) ("e" 5)))

(check-expect (argmax second one-list) '("e" 5))

; [X] [X -> Number] [NEList-of X] -> X
; finds the (first) item in lx that minimizes f
; if (argmin f (list x-1 ... x-n)) == x-i,
; then (<= (f x-i) (f x-1)), (<= (f x-i) (f x-2)), ...
; (define (argmin f lx) ...)
