#lang htdp/isl+

; [X Y] [X Y -> Y] Y [List-of X] -> Y
; applies f from left to right to each item in l0 and i
(define (f*ldl f i l0)
  (local (; Y [List-of X] -> Y
          ; accumulator a is the value produced by applying
          ; f on items that l lacks from l0 and a itself sequentially
          (define (foldr/a a l)
            (cond
              [(empty? l) a]
              [else
               (foldr/a (f (first l) a) (rest l))])))
    (foldr/a i l0)))

; N [N -> X] -> [List-of X]
; constructs a list by applying f to 0, 1, ..., (sub1 n0)
(check-expect (build-l*st 5 add1) (build-list 5 add1))
(define (build-l*st n0 f)
  (local (; N [List-of X] -> [List-of X]
          ; accumulator a is a list by applying f
          ; to n, ..., (sub1 n0)
          (define (build-l*st/a n a)
            (cond
              [(zero? n) (cons (f n) a)]
              [else
               (build-l*st/a (sub1 n) (cons (f n) a))])))
    (build-l*st/a (sub1 n0) '())))
