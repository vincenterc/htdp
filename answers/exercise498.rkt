#lang htdp/isl+

(define-struct node [left right])
; A Tree is one of:
; – '()
; – (make-node Tree Tree)
(define example
  (make-node (make-node '() (make-node '() '())) '()))

; Tree -> N
; measures the height of abt0
(check-expect (height.v3 '()) 0)
(check-expect (height.v3 (make-node '() '())) 1)
(check-expect (height.v3 example) 3)
(define (height.v3 abt0)
  (local (; Tree N N -> N
          ; measures the height of abt
          ; accumulator s is the number of steps
          ; it takes to reach abt from abt0
          ; accumulator m is the maximal height of
          ; the part of abt0 that is to the left of abt
          (define (h/a abt s m)
            (cond
              [(empty? abt) s]
              [else
               (max (h/a (node-left abt) (add1 s) (add1 m))
                    (h/a (node-right abt) (add1 s) m))])))
    (h/a abt0 0 0)))
