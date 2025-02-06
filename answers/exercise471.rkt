#lang htdp/isl+

; A Graph is [List-of NN]
; An NN (Name and Neighbors) is a list of two items:
;   (list Node [List-of Node])
; interpretation the first item is the name of a node and
; the second one contains its (immediate) neighbors
; A Node is a Symbol.

(define sample-graph
  (list (list 'A (list 'B 'E))
        (list 'B (list 'E 'F))
        (list 'C (list 'D))
        (list 'D '())
        (list 'E (list 'C 'F))
        (list 'F (list 'D 'G))
        (list 'G '())))

; Node Graph -> [List-of Node]
; finds the list of immediate neighbors of n in g
(check-expect (neighbors 'A '()) '())
(check-expect (neighbors 'A sample-graph) '(B E))
(check-expect (neighbors 'D sample-graph) '())
(define (neighbors n g)
  (local (; [Maybe NN]
          (define nn (assq n g)))
    (if (boolean? nn)
        '()
        (second nn))))
