#lang htdp/bsl

(require 2htdp/image)

(define ROWS 20)
(define COLUMNS 10)

(define SQUARE-SIZE 10)
(define SQUARE (square SQUARE-SIZE "outline" "black"))

(define BALLOON (circle 3 "solid" "red"))

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

(define LECTURE-HALL
  (overlay (col ROWS (row COLUMNS SQUARE))
           (empty-scene (* SQUARE-SIZE COLUMNS) (* SQUARE-SIZE ROWS))))

; A List-of-posns is one of:
; – '()
; – (cons Posn List-of-posns)
; interpretation a list of Posn structures

; List-of-posns -> Image
; adds red dots, with coordinates specified by the Posn structures in l,
; to the image of the lecture hall
(check-expect (add-balloons '()) LECTURE-HALL)
(check-expect (add-balloons (cons (make-posn 20 30)
                                  (cons (make-posn 90 30) '())))
              (place-image BALLOON 20 30
                           (place-image BALLOON 90 30 LECTURE-HALL)))
(define (add-balloons l)
  (cond [(empty? l) LECTURE-HALL]
        [else (place-image
               BALLOON
               (posn-x (first l)) (posn-y (first l))
               (add-balloons (rest l)))]))
