#lang htdp/isl

; [X] N [N -> X] -> [List-of X]
; constructs a list by applying f to 0, 1, ..., (sub1 n)
; (build-l*st n f) == (list (f 0) ... (f (- n 1)))
(check-expect (build-l*st 0 add1) '())
(check-expect (build-l*st 1 add1) '(1))
(check-expect (build-l*st 2 add1) '(1 2))
(check-expect (build-l*st 3 add1)
              (build-list 3 add1))
(define (build-l*st n f)
  (cond [(zero? n) '()]
        [else (add-at-end (build-l*st (sub1 n) f) (f (sub1 n)))]))

; [X] [list-of X] X -> [List-of X]
; creates a new list by adding x to the end of lox
(define (add-at-end lox x)
  (cond [(empty? lox) (cons x '())]
        [else (cons (first lox) (add-at-end (rest lox) x))]))
