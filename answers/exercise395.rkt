#lang htdp/isl+

; N is one of:
; – 0
; – (add1 N)

; [List-of X] N -> [List-of X]
; produces the first n items from l or all of l if it is too short
(check-expect (take '() 0) '())
(check-expect (take '() 2) '())
(check-expect (take '(1 2 3) 0) '())
(check-expect (take '(1 2 3) 1) '(1))
(check-expect (take '(1 2 3) 2) '(1 2))
(define (take l n)
  (cond [(empty? l) '()]
        [(= n 0) '()]
        [(> n 0)
         (cons (first l)
               (take (rest l) (sub1 n)))]))

; [List-of X] N -> [List-of X]
; produces l with the first n items removed
; '() if l is too short
(check-expect (drop '() 0) '())
(check-expect (drop '() 2) '())
(check-expect (drop '(1 2 3) 0) '(1 2 3))
(check-expect (drop '(1 2 3) 1) '(2 3))
(check-expect (drop '(1 2 3) 2) '(3))
(define (drop l n)
  (cond [(empty? l) '()]
        [(= n 0) l]
        [(> n 0) (drop (rest l) (sub1 n))]))
