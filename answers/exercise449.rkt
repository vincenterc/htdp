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
; termination each recursive call the interval is divided in half
(check-within (find-root poly 0 3) 2 ε)
(check-within (find-root poly 3 6) 4 ε)
(define (find-root f left right)
  (local (; Number Number Number Number -> Number
          (define (find-root-helper l f@l r f@r)
            (cond
              [(<= (- r l) ε) l]
              [else
               (local ((define mid (/ (+ l r) 2))
                       (define f@mid (f mid)))
                 (cond
                   [(or (<= f@l 0 f@mid) (<= f@mid 0 f@l))
                    (find-root-helper l f@l mid f@mid)]
                   [(or (<= f@mid 0 f@r) (<= f@r 0 f@mid))
                    (find-root-helper mid f@mid r f@r)]))])))
    (find-root-helper left (f left) right (f right))))
