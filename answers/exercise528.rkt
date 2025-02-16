#lang htdp/isl+

(require 2htdp/image)

(define SMALL 5)
(define COLOR "red")

; Image Posn Posn Posn -> Image
; generative takes the triangle (a, b, c) and subdivides it into
; two triangles by taking the midpoints of the side ab and bc, A-B
; and B-C, and the midpoints of the line between A-B amd B-C;
; stop and draw the line between a and c if (a, b, c) is too small
; accumulator the function accumulates the empty scene scene0
(define (add-bezier scene0 a b c)
  (cond
    [(too-small? a b c)
     (scene+line
      scene0 (posn-x a) (posn-y a) (posn-x c) (posn-y c) COLOR)]
    [else
     (local
       ((define mid-a-b (mid-point a b))
        (define mid-b-c (mid-point b c))
        (define mid-a-b-c (mid-point mid-a-b mid-b-c))
        (define scene1
          (add-bezier scene0 a mid-a-b mid-a-b-c)))
       ; -IN-
       (add-bezier scene1 mid-a-b-c mid-b-c c))]))

; Posn Posn Posn -> Boolean
; is the triangle a, b, c too small to be divided
(define (too-small? a b c)
  (or (<= (distance a b) SMALL)
      (<= (distance b c) SMALL)
      (<= (distance c a) SMALL)))

; Posn Posn -> Number
; calculates the distance between p1 and p2
(define (distance p1 p2)
  (sqrt (+ (sqr (- (posn-x p1) (posn-x p2)))
           (sqr (- (posn-y p1) (posn-y p2))))))

; Posn Posn -> Posn
; determines the midpoint between a and b
(define (mid-point a b)
  (make-posn (* 1/2 (+ (posn-x a) (posn-x b)))
             (* 1/2 (+ (posn-y a) (posn-y b)))))

; (add-bezier (empty-scene 200 200)
;             (make-posn 10 10)
;             (make-posn 50 190)
;             (make-posn 150 100))
