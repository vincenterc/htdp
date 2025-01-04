#lang htdp/isl+

(define (f-plain x)
  (* 10 x))

(define f-lambda
  (lambda (x)
    (* 10 x)))

; Number -> Boolean
(define (compare x)
  (= (f-plain x) (f-lambda x)))

; (compare (random 100000))
