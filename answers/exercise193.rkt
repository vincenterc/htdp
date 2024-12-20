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
 (render-poly.v2 MT triangle-p)
 (scene+line
  (scene+line
   (scene+line MT 20 10 20 20 "red")
   20 20 30 20 "red")
  30 20 20 10 "red"))
(check-expect
 (render-poly.v2 MT square-p)
 (scene+line
  (scene+line
   (scene+line
    (scene+line MT 10 10 20 10 "red")
    20 10 20 20 "red")
   20 20 10 20 "red")
  10 20 10 10 "red"))
(define (render-poly.v2 img p)
  (connect-dots
   img
   (cons (last p) p)))

; Image Polygon -> Image
; adds an image of p to img
(check-expect
 (render-poly.v3 MT triangle-p)
 (scene+line
  (scene+line
   (scene+line MT 20 10 20 20 "red")
   20 20 30 20 "red")
  30 20 20 10 "red"))
(check-expect
 (render-poly.v3 MT square-p)
 (scene+line
  (scene+line
   (scene+line
    (scene+line MT 10 10 20 10 "red")
    20 10 20 20 "red")
   20 20 10 20 "red")
  10 20 10 10 "red"))
(define (render-poly.v3 img p)
  (connect-dots
   img
   (cons (last p) p)))

; Image NELoP -> Image
; connects the dots in p by rendering lines in img
(check-expect (connect-dots MT triangle-p)
              (scene+line
               (scene+line MT 20 20 30 20 "red")
               20 10 20 20 "red"))
(check-expect (connect-dots MT square-p)
              (scene+line
               (scene+line
                (scene+line MT 20 20 10 20 "red")
                20 10 20 20 "red")
               10 10 20 10 "red"))
(define (connect-dots img p)
  (cond
    [(empty? (rest p)) img]
    [else (render-line (connect-dots img (rest p))
                       (first p)
                       (second p))]))

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

; Polygon -> Posn
; extracts the last item from p
(check-expect (last triangle-p) (make-posn 30 20))
(check-expect (last square-p) (make-posn 10 20))
(define (last p)
  (cond
    [(empty? (rest (rest (rest p)))) (third p)]
    [else (last (rest p))]))

; Polygon Posn -> Lo1s
; creates a new list by adding p to the end of l
(check-expect
 (add-at-end triangle-p (make-posn 20 10))
 (list
  (make-posn 20 10)
  (make-posn 20 20)
  (make-posn 30 20)
  (make-posn 20 10)))
(check-expect
 (add-at-end square-p (make-posn 10 10))
 (list
  (make-posn 10 10)
  (make-posn 20 10)
  (make-posn 20 20)
  (make-posn 10 20)
  (make-posn 10 10)))
(define (add-at-end l p)
  (cond
    [(empty? l) (cons p '())]
    [else
     (cons (first l) (add-at-end (rest l) p))]))
