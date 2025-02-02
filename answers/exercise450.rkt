#lang htdp/isl+

(define ε 0.0001)

; Number -> Number
(define (line x)
  (- x 2))

; [Number -> Number] Number Number -> Number
; determines R such that f has a root in [R,(+ R ε)]
; assume f is continuous
; assume f is monotonically increasing
; (if (<= (f a) (f b)) holds whenever (< a b) holds)
; assume (<= (f left) 0 (f right))
; generative divides interval in half, the root is in one of the two
; halves, picks according to assumption
; termination each recursive call the interval is divided in half
(check-within (find-root line 0 4) 2 ε)
(define (find-root f left right)
  (cond
    [(<= (- right left) ε) left]
    [else
     (local ((define mid (/ (+ left right) 2))
             (define f@mid (f mid)))
       (if (> f@mid 0)
           (find-root f left mid)
           (find-root f mid right)))]))
