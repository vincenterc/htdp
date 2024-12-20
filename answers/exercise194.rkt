#lang htdp/bsl+

(require 2htdp/image)

; A Polygon is one of:
; – (list Posn Posn Posn)
; – (cons Posn Polygon)

; An NELoP is one of:
; – (cons Posn '())
; – (cons Posn NELoP)

(define triangle-p
  (list
   (make-posn 20 10)
   (make-posn 20 20)
   (make-posn 30 20)))

(define square-p
  (list
   (make-posn 10 10)
   (make-posn 20 10)
   (make-posn 20 20)
   (make-posn 10 20)))

; a plain background image
(define MT (empty-scene 50 50))

; Image Polygon -> Image
; adds an image of p to img
(check-expect
 (render-poly.v4 MT triangle-p)
 (scene+line
  (scene+line
   (scene+line MT 20 10 20 20 "red")
   20 20 30 20 "red")
  30 20 20 10 "red"))
(check-expect
 (render-poly.v4 MT square-p)
 (scene+line
  (scene+line
   (scene+line
    (scene+line MT 10 10 20 10 "red")
    20 10 20 20 "red")
   20 20 10 20 "red")
  10 20 10 10 "red"))
(define (render-poly.v4 img p)
  (connect-dots.v2 img p (first p)))

; Image NELoP Posn -> Image
; connects the dots in l and also connects p to the last Posn in l
; by rendering lines in img
(check-expect (connect-dots.v2 MT triangle-p (make-posn 20 10))
              (scene+line
               (scene+line
                (scene+line MT 30 20 20 10 "red")
                20 20 30 20 "red")
               20 10 20 20 "red"))
(check-expect (connect-dots.v2 MT square-p (make-posn 10 10))
              (scene+line
               (scene+line
                (scene+line
                 (scene+line MT 10 20 10 10 "red")
                 20 20 10 20 "red")
                20 10 20 20 "red")
               10 10 20 10 "red"))
(define (connect-dots.v2 img l p)
  (cond
    [(empty? (rest l))
     (render-line img (first l) p)]
    [else (render-line
           (connect-dots.v2 img (rest l) p)
           (first l)
           (second l))]))

; Image Posn Posn -> Image
; renders a line from p to q into img
(check-expect
 (render-line MT (make-posn 10 10) (make-posn 20 10))
 (scene+line MT 10 10 20 10 "red"))
(define (render-line img p q)
  (scene+line
   img
   (posn-x p) (posn-y p) (posn-x q) (posn-y q)
   "red"))
