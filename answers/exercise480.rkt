#lang htdp/isl+

(require 2htdp/image)

; A QP is a structure:
;   (make-posn CI CI)
; A CI is an N in [0,QUEENS).
; interpretation (make-posn r c) denotes the square at
; the r-th row and c-th column

(define QUEEN (text "Q" 16 "black"))

(define CELL-SIZE 20)
(define HALF-CELL-SIZE (/ CELL-SIZE 2))
(define CELL (square CELL-SIZE "outline" "black"))

; N [List-of QP] Image -> Image
; produces an image of an n by n chess board
; with img placed according to loqp
(define (render-queens n loqp img)
  (local ((define row
            (foldr beside empty-image
                   (build-list n (lambda (n) CELL))))
          (define board
            (foldr above empty-image
                   (build-list n (lambda (n) row))))
          (define (qp->pos qp)
            (make-posn (+ (* (posn-x qp) CELL-SIZE)
                          HALF-CELL-SIZE)
                       (+ (* (posn-y qp) CELL-SIZE)
                          HALF-CELL-SIZE)))
          (define lop (map qp->pos loqp)))
    (foldr (lambda (p scene)
             (place-image QUEEN (posn-x p) (posn-y p) scene))
           board lop)))
