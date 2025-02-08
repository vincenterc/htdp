#lang htdp/isl+

; A QP is a structure:
;   (make-posn CI CI)
; A CI is an N in [0,QUEENS).
; interpretation (make-posn r c) denotes the square at
; the r-th row and c-th column

; Board N -> [Maybe [List-of QP]]
; places n queens on board; otherwise, returns #false
(define (place-queens a-board n)
  (cond
    [(= n 0) '()]
    [else
     (local (; [List-of QP] -> [Maybe [List-of QP]]
             (define (place-queens/list loqp)
               (cond
                 [(empty? loqp) #false]
                 [else
                  (local ((define first-loqp (first loqp))
                          ; [Maybe [List-of QP]]
                          (define candidate
                            (place-queens
                             (add-queen a-board first-loqp)
                             (sub1 n))))
                    (cond [(boolean? candidate)
                           (place-queens/list (rest loqp))]
                          [else (cons first-loqp candidate)]))])))
       (place-queens/list (find-open-spots a-board)))]))

; Board QP -> Board
; places a queen at qp on a-board
(define (add-queen a-board qp)
  a-board)

; Board -> [List-of QP]
; finds spots where it is still safe to place a queen
(define (find-open-spots a-board)
  '())
