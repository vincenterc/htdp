#lang htdp/isl+

(define ε 0.0001)

; Number -> Number
(define (poly x)
  (* (- x 2) (- x 4)))

; [Number -> Number] Number Number -> Number
; determines R such that f has a root in [R,(+ R ε)]
; assume f is continuous
; assume (or (<= (f left) 0 (f right)) (<= (f right) 0 (f left)))
; generative divides interval in half, the root is in one of the two
; halves, picks according to assumption
(check-within (find-root poly 0 3) 2 ε)
(check-within (find-root poly 3 6) 4 ε)
(define (find-root f left right)
  (cond
    [(<= (- right left) ε) left]
    [else
     (local ((define mid (/ (+ left right) 2))
             (define f@mid (f mid)))
       (cond
         [(or (<= (f left) 0 f@mid) (<= f@mid 0 (f left)))
          (find-root f left mid)]
         [(or (<= f@mid 0 (f right)) (<= (f right) 0 f@mid))
          (find-root f mid right)]))]))

; ε        (find-root poly 3 6)
; 0.1      3.9375
; 0.01     3.99609375
; 0.001    3.999755859375
; 0.0001   3.99993896484375
