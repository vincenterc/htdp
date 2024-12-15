#lang htdp/bsl

(require 2htdp/image)
(require 2htdp/universe)

(define ROWS 20)
(define COLUMNS 10)

(define SQUARE-SIZE 10)
(define SQUARE (square SQUARE-SIZE "outline" "black"))

(define BACKGROUND-WIDTH (* SQUARE-SIZE COLUMNS))
(define BACKGROUND-HEIGHT (* SQUARE-SIZE ROWS))

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
           (empty-scene BACKGROUND-WIDTH BACKGROUND-HEIGHT)))

; A List-of-posns is one of:
; – '()
; – (cons Posn List-of-posns)
; interpretation a list of Posn structures

(define-struct pair [balloon# lop])
; A Pair is a structure (make-pair N List-of-posns)
; interpretation (make-pair n lop) means n balloons
; must yet be thrown and added to lop

; N -> Pair
(define (riot n)
  (big-bang (make-pair n '())
    [to-draw render]
    [on-tick throw 1]))

; Pair -> Image
; produces an image of the lecture hall with red dots
(check-expect (render (make-pair 10 '())) (add-balloons '()))
(check-expect (render (make-pair 9 (cons (make-posn 20 30) '())))
              (add-balloons (cons (make-posn 20 30) '())))
(define (render p)
  (add-balloons (pair-lop p)))

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

; Pair -> Pair
; throws one balloon per tick
(check-random (throw (make-pair 10 '()))
              (make-pair
               9
               (cons (make-posn (random BACKGROUND-WIDTH)
                                (random BACKGROUND-HEIGHT))
                     '())))
(check-random (throw (make-pair
                      9
                      (cons (make-posn 90 30) '())))
              (make-pair
               8
               (cons (make-posn (random BACKGROUND-WIDTH)
                                (random BACKGROUND-HEIGHT))
                     (cons (make-posn 90 30) '()))))
(define (throw p)
  (cond [(zero? (pair-balloon# p)) p]
        [(positive? (pair-balloon# p))
         (make-pair (sub1 (pair-balloon# p))
                    (cons (make-posn (random BACKGROUND-WIDTH)
                                     (random BACKGROUND-HEIGHT))
                          (pair-lop p)))]))

; Application
; (riot 10)
