#lang htdp/bsl+

(require 2htdp/image)

; An NELoP is one of:
; – (cons Posn '())
; – (cons Posn NELoP)

(define square-p
  (list
   (make-posn 10 10)
   (make-posn 20 10)
   (make-posn 20 20)
   (make-posn 10 20)))

; a plain background image
(define MT (empty-scene 50 50))

; Image NELoP -> Image
; connects the dots in p by rendering lines in img
(check-expect (connect-dots MT square-p)
              (scene+line
               (scene+line
                (scene+line MT 20 20 10 20 "red")
                20 10 20 20 "red")
               10 10 20 10 "red"))
(define (connect-dots img p)
  (cond
    [(empty? (rest p)) img]
    [else
     (render-line
      (connect-dots img (rest p))
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
