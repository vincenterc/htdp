#lang htdp/bsl

(require 2htdp/image)

; An N is one of:
; – 0
; – (add1 N)
; interpretation represents the counting numbers

; N Image -> Image
; produces a column of n copies of img
(check-expect (col 0 (rectangle 10 20 "solid" "red"))
              empty-image)
(check-expect (col 1 (rectangle 10 20 "solid" "red"))
              (rectangle 10 20 "solid" "red"))
(check-expect (col 2 (circle 10 "solid" "blue"))
              (above (circle 10 "solid" "blue")
                     (circle 10 "solid" "blue")))
(check-expect (col 3 (square 10 "solid" "green"))
              (above (square 10 "solid" "green")
                     (square 10 "solid" "green")
                     (square 10 "solid" "green")))
(define (col n img)
  (cond [(zero? n) empty-image]
        [(positive? n) (above img (col (sub1 n) img))]))

; N Image -> Image
; produces a row of n copies of img
(check-expect (row 0 (rectangle 10 20 "solid" "red"))
              empty-image)
(check-expect (row 1 (rectangle 10 20 "solid" "red"))
              (rectangle 10 20 "solid" "red"))
(check-expect (row 2 (circle 10 "solid" "blue"))
              (beside (circle 10 "solid" "blue")
                      (circle 10 "solid" "blue")))
(check-expect (row 3 (square 10 "solid" "green"))
              (beside (square 10 "solid" "green")
                      (square 10 "solid" "green")
                      (square 10 "solid" "green")))
(define (row n img)
  (cond [(zero? n) empty-image]
        [(positive? n) (beside img (row (sub1 n) img))]))
