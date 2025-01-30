#lang htdp/isl+

(define MAX 17)

; Posn -> Posn
; creates a piece of food at a random position that is not on the
; position p
(check-satisfied (food-create (make-posn 1 1)) not=-1-1?)
(define (food-create p)
  (local (; Posn Posn -> Posn
          ; generative recursion
          ; checks if the candidate position is on the positions p0
          ; If so, recreate a position.
          (define (food-check-create p0 candidate)
            (if (equal? p0 candidate) (food-create p0) candidate)))
    (food-check-create
     p (make-posn (random MAX) (random MAX)))))

; Posn -> Boolean
; use for testing only
(define (not=-1-1? p)
  (not (and (= (posn-x p) 1) (= (posn-y p) 1))))
