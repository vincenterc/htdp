#lang htdp/bsl

; A List-of-posns is one of:
; – '()
; – (cons Posn List-of-posns)
; interpretation a list of Posn structures

; List-of-posns -> Number
; produces the sum of all x-coordinates in l
(check-expect (sum '())
              0)
(check-expect (sum (cons (make-posn 1 1) '()))
              1)
(check-expect (sum (cons (make-posn 2 2)
                         (cons (make-posn 1 1) '())))
              3)
(define (sum l)
  (cond [(empty? l) 0]
        [else (+ (posn-x (first l))
                 (sum (rest l)))]))
