#lang htdp/isl

; A Matrix is one of:
; – (cons Row '())
; – (cons Row Matrix)
; constraint all rows in matrix are of the same length

; A Row is one of:
; – '()
; – (cons Number Row)

; PositiveInteger -> Matrix
; creates a diagonal square of size n0
(check-expect (identityM 1)
              (list (list 1)))
(check-expect (identityM 3)
              (list (list 1 0 0) (list 0 1 0) (list 0 0 1)))
(define (identityM n0)
  (local (; PositiveInteger -> Row
          ; produces the first row of a diagonal square
          (define (first-row n)
            (cond [(= n 1) (list 1)]
                  [else (add-at-end (first-row (sub1 n)) 0)]))

          ; Matrix -> Matrix
          ; produces a matrix by add 0 to the beginning of
          ; all rows of the given matrix lor
          (define (add-0-at-beginning/all-rows lor)
            (cond [(empty? lor) '()]
                  [else
                   (cons (cons 0 (first lor))
                         (add-0-at-beginning/all-rows (rest lor)))])))
    (cond [(= n0 1) (cons (first-row n0) '())]
          [else
           (cons (first-row n0)
                 (add-0-at-beginning/all-rows
                  (identityM (sub1 n0))))])))

; [X] [list-of X] X -> [List-of X]
; creates a new list by adding x to the end of lox
(define (add-at-end lox x)
  (cond [(empty? lox) (cons x '())]
        [else (cons (first lox) (add-at-end (rest lox) x))]))
