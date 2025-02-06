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

; A Path is a [List-of Node].
; interpretation The list of nodes specifies a sequence
; of immediate neighbors that leads from the first
; Node on the list to the last one.

; Node Node Graph -> [Maybe Path]
; finds a path from origination to destination in G
; if there is no path, the function produces #false
(check-expect (find-path 'C 'D sample-graph)
              '(C D))
(check-member-of (find-path 'E 'D sample-graph)
                 '(E F D) '(E C D))
(check-expect (find-path 'C 'G sample-graph)
              #false)
(check-expect (find-path 'A 'G sample-graph)
              '(A B E F G))
(define (find-path origination destination G)
  (cond
    [(symbol=? origination destination) (list destination)]
    [else (local ((define next (neighbors origination G))
                  (define candidate
                    (find-path/list next destination G)))
            (cond
              [(boolean? candidate) #false]
              [else (cons origination candidate)]))]))

; [List-of Node] Node Graph -> [Maybe Path]
; finds a path from some node on lo-Os to D
; if there is no path, the function produces #false
(define (find-path/list lo-Os D G)
  (cond
    [(empty? lo-Os) #false]
    [else (local ((define candidate
                    (find-path (first lo-Os) D G)))
            (cond
              [(boolean? candidate)
               (find-path/list (rest lo-Os) D G)]
              [else candidate]))]))

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

; Graph -> Boolean
; determines whether there is a path between every pair of nodes in g
(check-expect (test-on-all-nodes sample-graph) #false)
(define (test-on-all-nodes g)
  (local ((define all-nodes
            (map (lambda (nn) (first nn)) g))
          (define all-pairs
            (foldr
             (lambda (n1 ap)
               (append
                (foldr (lambda (n2 a)
                         (if (symbol=? n1 n2) a (cons (list n1 n2) a)))
                       '() all-nodes)
                ap))
             '() all-nodes)))
    (andmap (lambda (p) (cons? (find-path (first p) (second p) g)))
            all-pairs)))
