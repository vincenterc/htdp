#lang htdp/isl+

(require 2htdp/image)

(define SMALL 10)
(define COLOR "red")

; Image Posn Posn Posn -> Image
; adds the triangle a, b, c to scene
(define (add-triangle scene a b c)
  (local ((define s1
            (scene+line
             scene (posn-x a) (posn-y a) (posn-x b) (posn-y b) COLOR))
          (define s2
            (scene+line
             s1 (posn-x b) (posn-y b) (posn-x c) (posn-y c) COLOR)))
    (scene+line
     s2 (posn-x c) (posn-y c) (posn-x a) (posn-y a) COLOR)))

; Posn Posn Posn -> Boolean
; is the triangle a, b, c too small to be divided
(check-expect
 (too-small? (make-posn 0 0) (make-posn 12 0) (make-posn 6 10.39))
 #false)
(check-expect
 (too-small? (make-posn 0 0) (make-posn 8 0) (make-posn 4 6.93))
 #true)
(check-expect
 (too-small? (make-posn 0 0) (make-posn 8 0) (make-posn 0 12))
 #true)
(define (too-small? a b c)
  (or (<= (distance a b) SMALL)
      (<= (distance b c) SMALL)
      (<= (distance c a) SMALL)))

; Posn Posn -> Number
; calculates the distance between p1 and p2
(check-expect (distance (make-posn 0 0) (make-posn 4 0)) 4)
(check-expect (distance (make-posn 0 0) (make-posn 0 5)) 5)
(check-expect (distance (make-posn 0 0) (make-posn 3 4)) 5)
(define (distance p1 p2)
  (sqrt (+ (sqr (- (posn-x p1) (posn-x p2)))
           (sqr (- (posn-y p1) (posn-y p2))))))

; Posn Posn -> Posn
; determines the midpoint between a and b
(check-expect
 (mid-point (make-posn 0 0) (make-posn 10 0))
 (make-posn 5 0))
(check-expect
 (mid-point (make-posn 0 0) (make-posn 0 8))
 (make-posn 0 4))
(check-expect
 (mid-point (make-posn 0 0) (make-posn 4 4))
 (make-posn 2 2))
(define (mid-point a b)
  (make-posn (* 1/2 (+ (posn-x a) (posn-x b)))
             (* 1/2 (+ (posn-y a) (posn-y b)))))
