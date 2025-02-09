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

(define cyclic-graph
  (list (list 'A (list 'B 'E))
        (list 'B (list 'E 'F))
        (list 'C (list 'B 'D))
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
    [else (local (; Node Graph -> [List-of Node]
                  ; finds the list of immediate neighbors of n in g
                  (define (neighbors n g)
                    (local (; [Maybe NN]
                            (define nn (assq n g)))
                      (if (boolean? nn)
                          '()
                          (second nn))))
                  ; [List-of Node] Node Graph -> [Maybe Path]
                  ; finds a path from some node on lo-Os to D
                  ; if there is no path, the function produces #false
                  (define (find-path/list lo-Os)
                    (foldl (lambda (o res)
                             (if (cons? res)
                                 res
                                 (find-path o destination G)))
                           #false
                           lo-Os))
                  (define next (neighbors origination G))
                  (define candidate
                    (find-path/list next)))
            (cond
              [(boolean? candidate) #false]
              [else (cons origination candidate)]))]))

; Racket's ormap returns first non-false value;
; however, ISL's ormap expects a boolean value.
; So Racket's ormap would be helper in this case.
