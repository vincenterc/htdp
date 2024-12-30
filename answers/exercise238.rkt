#lang htdp/isl

; Nelon -> Number
; determines the smallest
; number on l
(define (inf l)
  (cond
    [(empty? (rest l))
     (first l)]
    [else
     (if (< (first l)
            (inf (rest l)))
         (first l)
         (inf (rest l)))]))

; Nelon -> Number
; determines the largest
; number on l
(define (sup l)
  (cond
    [(empty? (rest l))
     (first l)]
    [else
     (if (> (first l)
            (sup (rest l)))
         (first l)
         (sup (rest l)))]))

(define (extremum R l)
  (cond
    [(empty? (rest l))
     (first l)]
    [else
     (if (R (first l)
            (extremum R (rest l)))
         (first l)
         (extremum R (rest l)))]))

; Nelon -> Number
; determines the smallest number on l
(define (inf-1 l)
  (extremum < l))
; (inf-1 (list 25 24 23 22 21 20 19 18 17 16 15 14 13
;       12 11 10 9 8 7 6 5 4 3 2 1))
; (inf-1 (list 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16
;              17 18 19 20 21 22 23 24 25))

; Nelon -> Number
; determines the largest number on l
(define (sup-1 l)
  (extremum > l))
; (sup-1 (list 25 24 23 22 21 20 19 18 17 16 15 14 13
;              12 11 10 9 8 7 6 5 4 3 2 1))
; (sup-1 (list 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16
;              17 18 19 20 21 22 23 24 25))

; Nelon -> Number
; determines the smallest
; number on l
(define (inf-min l)
  (cond
    [(empty? (rest l))
     (first l)]
    [else
     (min (first l)
          (inf-min (rest l)))]))

; Nelon -> Number
; determines the largest
; number on l
(define (sup-max l)
  (cond
    [(empty? (rest l))
     (first l)]
    [else
     (max (first l)
          (sup-max (rest l)))]))

(define (extremum-2 R l)
  (cond
    [(empty? (rest l))
     (first l)]
    [else
     (R (first l)
        (extremum-2 R (rest l)))]))

; Nelon -> Number
; determines the smallest number on l
(define (inf-2 l)
  (extremum-2 min l))
; (inf-2 (list 25 24 23 22 21 20 19 18 17 16 15 14 13
;       12 11 10 9 8 7 6 5 4 3 2 1))
; (inf-2 (list 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16
;              17 18 19 20 21 22 23 24 25))

; Nelon -> Number
; determines the largest number on l
(define (sup-2 l)
  (extremum-2 max l))
; (sup-2 (list 25 24 23 22 21 20 19 18 17 16 15 14 13
;              12 11 10 9 8 7 6 5 4 3 2 1))
; (sup-2 (list 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16
;              17 18 19 20 21 22 23 24 25))
