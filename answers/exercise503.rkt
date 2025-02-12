#lang htdp/isl+

; A Matrix is one of:
;  – (cons Row '())
;  – (cons Row Matrix)
; constraint all rows in matrix are of the same length

; A Row is one of:
;  – '()
;  – (cons Number Row)

(define ALL-ZERO "All rows start with 0")

; Matrix -> Matrix
; finds a row that doesn't start with 0 and
; uses it as the first one
; generative moves the first row to last place
; no termination if all rows start with 0
(check-expect (rotate '((0 4 5) (1 2 3)))
              '((1 2 3) (0 4 5)))
(check-expect (rotate '((0 4 5) (0 6 7) (1 2 3)))
              '((1 2 3) (0 4 5) (0 6 7)))
(check-error (rotate '((0 4 5) (0 6 7) (0 2 3)))
             ALL-ZERO)
(define (rotate M0)
  (local (; Matrix N -> Matrix
          ; finds a row that doesn't start with 0 and
          ; uses it as the first one
          ; accumulator N is the total number of rotations
          (define (rotate-aux M N)
            (cond
              [(zero? N) (error ALL-ZERO)]
              [(not (= (first (first M)) 0)) M]
              [else
               (rotate-aux (append (rest M) (list (first M)))
                           (sub1 N))])))
    (rotate-aux M0 (length M0))))

; Matrix -> Matrix
; finds a row that doesn't start with 0 and
; uses it as the first one
(check-expect (rotate.v2 '((0 4 5) (1 2 3)))
              '((1 2 3) (0 4 5)))
(check-expect (rotate.v2 '((0 4 5) (0 6 7) (1 2 3)))
              '((1 2 3) (0 4 5) (0 6 7)))
(check-error (rotate.v2 '((0 4 5) (0 6 7) (0 2 3)))
             ALL-ZERO)
(define (rotate.v2 M0)
  (local (; Matrix [List-of Row] -> Matrix
          ; accumulator seen is a list of Rows that
          ; M lacks from M0 in reverse order
          (define (rotate/a M seen)
            (cond
              [(empty? M)
               (error ALL-ZERO)]
              [(not (= (first (first M)) 0))
               (append M (reverse seen))]
              [else
               (rotate/a (rest M) (cons (first M) seen))])))
    (rotate/a M0 '())))

; N -> Matrix
; creates a Matrix that consists of n rows
; with leading 0s except for the last one
(check-expect (matrix 0) '())
(check-expect (matrix 1) '((1 1 1)))
(check-expect (matrix 2)
              '((0 1 1)
                (1 1 1)))
(check-expect (matrix 3)
              '((0 1 1)
                (0 1 1)
                (1 1 1)))
(define (matrix n)
  (build-list n (lambda (i) (if (= i (sub1 n)) '(1 1 1) '(0 1 1)))))

; (time (rotate (matrix n)))
; rows in M 1000 2000 3000 4000 5000
; rotate      10   45   93  156  250

; (time (rotate.v2 (matrix n)))
; rows in M 1000 2000 3000 4000 5000
; rotate.v2    0    1    1    2    3
