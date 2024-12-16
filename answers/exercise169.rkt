#lang htdp/bsl

; A List-of-posns is one of:
; – '()
; – (cons Posn List-of-posns)
; interpretation a list of Posn structures

; List-of-posns -> List-of-posns
; add 1 to the y component of each posn in l
(check-expect (legal '())
              '())
(check-expect (legal (cons (make-posn 1 1) '()))
              (cons (make-posn 1 1) '()))
(check-expect (legal (cons (make-posn 102 2)
                           (cons (make-posn 1 1) '())))
              (cons (make-posn 1 1) '()))
(check-expect (legal (cons (make-posn 2 102)
                           (cons (make-posn 1 1) '())))
              (cons (make-posn 1 1) '()))
(check-expect (legal (cons (make-posn 2 102)
                           (cons (make-posn 101 1) '())))
              '())
(define (legal l)
  (cond [(empty? l) '()]
        [else (if (legal? (first l))
                  (cons (first l) (legal (rest l)))
                  (legal (rest l)))]))

; Posn -> Boolean
; determines if p is legal
(define (legal? p)
  (and (<= 0 (posn-x p))
       (<= (posn-x p) 100)
       (<= 0 (posn-y p))
       (<= (posn-y p) 100)))
