#lang htdp/isl+

(require 2htdp/image)

(define SMALL 10)
(define COLOR "red")
(define DANGLE-L 0.15) ; radian
(define DANGLE-R -0.2)
(define LENGTH-FACTOR-L 0.66)
(define LENGTH-FACTOR-R 0.8)

; Image Number Number Number Number -> Image
; generative adds the line (x, y, length, angle) to scene0,
; subdivides it into three parts, adds two new lines by taking
; two intermediate points as new starting points and by changing
; length and angle in a fixed manner independently; stop
; if length is too small
; accumulator the function accumulates the line of scene0
(define (add-savannah scene0 x y length angle)
  (cond
    [(<= length SMALL) scene0]
    [else
     (local
       ((define dx (* length (cos angle)))
        (define dy (* length (sin angle)))
        (define scene1
          (scene+line
           scene0 x y (+ x dx) (+ y dy) COLOR))
        (define scene2
          (add-savannah
           scene1 (+ x (* dx 2/3)) (+ y (* dy 2/3))
           (* length LENGTH-FACTOR-L) (+ angle DANGLE-L))))
       ; -IN-
       (add-savannah
        scene2 (+ x (* dx 1/3)) (+ y (* dy 1/3))
        (* length LENGTH-FACTOR-R) (+ angle DANGLE-R)))]))

; (add-savannah (empty-scene 200 200) 100 200 100 (* pi 3/2))
