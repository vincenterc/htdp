#lang htdp/isl+

; distances in terms of pixels
(define WIDTH 300)
(define HEIGHT 300)

; N -> [List-of Posn]
; generates n random Posns in [0,WIDTH) by [0,HEIGHT)
(check-satisfied (random-posns 3)
                 (n-inside-playground? 3))
(define (random-posns n)
  (build-list
   n
   (lambda (i)
     (make-posn (random WIDTH) (random HEIGHT)))))

; N -> [[List-of Posn] -> Boolean]
; is the length of l n
; are all Posns in l within a WIDTH by HEIGHT rectangle
(define (n-inside-playground? n)
  (lambda (l)
    (and (= (length l) n)
         (local (; Posn -> Boolean
                 ; determines p is within a WIDTH by HEIGHT rectangle
                 (define (within-rectangle? p)
                   (local ((define px (posn-x p))
                           (define py (posn-y p)))
                     (and (<= 0 px)
                          (< px WIDTH)
                          (<= 0 py)
                          (< py HEIGHT)))))
           (andmap within-rectangle? l)))))

; N -> [List-of Posn]
; generates n BAD random Posns in [0,WIDTH) by [0,HEIGHT)
(check-satisfied (random-posns/bad 3)
                 (n-inside-playground? 3))
(define (random-posns/bad n)
  (build-list
   n
   (lambda (i)
     (make-posn (/ WIDTH (+ i 2)) (/ HEIGHT (+ i 2))))))
