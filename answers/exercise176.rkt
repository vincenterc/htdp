#lang htdp/bsl

; A Matrix is one of:
;  – (cons Row '())
;  – (cons Row Matrix)
; constraint all rows in matrix are of the same length

; A Row is one of:
;  – '()
;  – (cons Number Row)

(define row1 (cons 11 (cons 12 '())))
(define row2 (cons 21 (cons 22 '())))
(define mat1 (cons row1 (cons row2 '())))

(define wor1 (cons 11 (cons 21 '())))
(define wor2 (cons 12 (cons 22 '())))
(define tam1 (cons wor1 (cons wor2 '())))

(define row3 (cons 11 (cons 12 (cons 13 '()))))
(define row4 (cons 21 (cons 22 (cons 23 '()))))
(define row5 (cons 31 (cons 32 (cons 33 '()))))
(define mat2 (cons row3 (cons row4 (cons row5 '()))))

(define wor3 (cons 11 (cons 21 (cons 31 '()))))
(define wor4 (cons 12 (cons 22 (cons 32 '()))))
(define wor5 (cons 13 (cons 23 (cons 33 '()))))
(define tam2 (cons wor3 (cons wor4 (cons wor5 '()))))

; Matrix -> Matrix
; transposes the given matrix along the diagonal
(check-expect (transpose mat1) tam1)
(check-expect (transpose mat2) tam2)
(define (transpose lln)
  (cond
    [(empty? (first lln)) '()]
    [else (cons (first* lln) (transpose (rest* lln)))]))

; Matrix -> Row
; produces the first column of lln as Row
(check-expect (first* mat1) wor1)
(check-expect (first* mat2) wor3)
(define (first* lln)
  (cond [(empty? lln) '()]
        [else (cons (first (first lln))
                    (first* (rest lln)))]))

; Matrix -> Matrix
; produces a matrix by consuming a matrix lln
; and removing its first column
(check-expect (rest* mat1) (cons (cons 12 '())
                                 (cons (cons 22 '())
                                       '())))
(check-expect (rest* mat2) (cons (cons 12 (cons 13 '()))
                                 (cons (cons 22 (cons 23 '()))
                                       (cons (cons 32 (cons 33 '()))
                                             '()))))
(define (rest* lln)
  (cond [(empty? lln) '()]
        [else (cons (rest (first lln))
                    (rest* (rest lln)))]))
