#lang htdp/bsl

; (define y 100)
(define y 210)

(- 200 (cond [(> y 200) 0] [else y]))
