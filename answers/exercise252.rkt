#lang htdp/isl

(require 2htdp/image)

; [List-of Number] -> Number
(check-expect (product '()) 1)
(check-expect (product (list 1)) 1)
(check-expect (product (list 2 1)) 2)
(check-expect (product (list 3 2 1)) 6)
; (define (product l)
;   (cond
;     [(empty? l) 1]
;     [else
;      (* (first l)
;         (product
;          (rest l)))]))
(define (product l)
  (fold2 l * 1))

; [List-of Posn] -> Image
(check-expect (image* '()) emt)
(check-expect
 (image* (list (make-posn 10 10)))
 (place-image dot 10 10 emt))
(check-expect
 (image* (list (make-posn 10 10) (make-posn 20 20)))
 (place-image dot 10 10
              (place-image dot 20 20 emt)))
; (define (image* l)
;   (cond
;     [(empty? l) emt]
;     [else
;      (place-dot
;       (first l)
;       (image* (rest l)))]))
(define (image* l)
  (fold2 l place-dot emt))

; Posn Image -> Image
(define (place-dot p img)
  (place-image
   dot
   (posn-x p) (posn-y p)
   img))

; graphical constants:
(define emt
  (empty-scene 100 100))
(define dot
  (circle 3 "solid" "red"))

(define (fold2 l f initial)
  (cond [(empty? l) initial]
        [else
         (f (first l)
            (fold2 (rest l) f initial))]))
