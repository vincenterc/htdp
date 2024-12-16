#lang htdp/bsl

; A List-of-posns is one of:
; – '()
; – (cons Posn List-of-posns)
; interpretation a list of Posn structures

; List-of-posns -> List-of-posns
; add 1 to the y component of each posn in l
(check-expect (translate '())
              '())
(check-expect (translate (cons (make-posn 1 1) '()))
              (cons (make-posn 1 2) '()))
(check-expect (translate (cons (make-posn 2 2)
                               (cons (make-posn 1 1) '())))
              (cons (make-posn 2 3)
                    (cons (make-posn 1 2) '())))
(define (translate l)
  (cond [(empty? l) '()]
        [else (cons (add1-y (first l))
                    (translate (rest l)))]))

; Posn -> Posn
; add 1 to the y component of p
(define (add1-y p)
  (make-posn (posn-x p)
             (add1 (posn-y p))))
