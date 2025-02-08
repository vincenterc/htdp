#lang htdp/isl+

; A QP is a structure:
;   (make-posn CI CI)
; A CI is an N in [0,QUEENS).
; interpretation (make-posn r c) denotes the square at
; the r-th row and c-th column

; QP QP -> Boolean
; determines whether two queens on qp1 and qp2
; would threaten each other
(check-expect (threatening? (make-posn 1 5) (make-posn 6 5)) #true)
(check-expect (threatening? (make-posn 1 5) (make-posn 1 2)) #true)
(check-expect (threatening? (make-posn 1 5) (make-posn 5 1)) #true)
(check-expect (threatening? (make-posn 1 5) (make-posn 3 7)) #true)
(check-expect (threatening? (make-posn 1 5) (make-posn 2 1)) #false)
(check-expect (threatening? (make-posn 1 5) (make-posn 6 2)) #false)
(check-expect (threatening? (make-posn 1 5) (make-posn 4 6)) #false)
(check-expect (threatening? (make-posn 1 5) (make-posn 7 7)) #false)
(define (threatening? qp1 qp2)
  (local ((define qp1x (posn-x qp1))
          (define qp1y (posn-y qp1))
          (define qp2x (posn-x qp2))
          (define qp2y (posn-y qp2)))
    (or (= qp1y qp2y)
        (= qp1x qp2x)
        (= (+ qp1x qp1y) (+ qp2x qp2y))
        (= (- qp1x qp1y) (- qp2x qp2y)))))
