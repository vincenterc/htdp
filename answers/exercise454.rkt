#lang htdp/isl+

; Number [List-of Number] -> [List-of [List-of Number]]
; converts a list of numbers into a list of lists of numbers
(check-expect
 (create-matrix 2 (list 1 2 3 4))
 (list (list 1 2)
       (list 3 4)))
(define (create-matrix n lon)
  (cond [(empty? lon) '()]
        [else (cons (take lon n)
                    (create-matrix n (drop lon n)))]))

; [List-of X] N -> [List-of X]
; keeps the first n items from l if possible or everything
(define (take l n)
  (cond
    [(zero? n) '()]
    [(empty? l) '()]
    [else (cons (first l) (take (rest l) (sub1 n)))]))

; [List-of X] N -> [List-of X]
; removes the first n items from l if possible or everything
(define (drop l n)
  (cond
    [(zero? n) l]
    [(empty? l) l]
    [else (drop (rest l) (sub1 n))]))
