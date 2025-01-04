#lang htdp/isl+

; N -> [List-of N]
; creates the list (list 0 ... (- n 1))
; for the given natural number n
(check-expect (list-n-1 5)
              '(0 1 2 3 4))
(define (list-n-1 n)
  (build-list n identity))

; N -> [List-of N]
; creates the list (list 1 ... n)
; for the given natural number n
(check-expect (list-n 5)
              '(1 2 3 4 5))
(define (list-n n)
  (build-list n add1))

; N -> [List-of Number]
; creates the list (list 1 1/2 ... 1/n)
; for the given natural number n
(check-expect (list-1/n 5)
              (list 1 (/ 1 2) (/ 1 3) (/ 1 4) (/ 1 5)))
(define (list-1/n n)
  (local (; Number -> Number
          ; produces a multiplicative inverse
          ; for a number n + 1
          (define (reciprocal-add1 n)
            (/ 1 (add1 n))))
    (build-list n reciprocal-add1)))

; N -> [List-of N]
; creates the list of the first n even numbers
(check-expect (list-even 1) '(0))
(check-expect (list-even 2) '(0 2))
(check-expect (list-even 3) '(0 2 4))
(define (list-even n)
  (local (; Number -> Number
          ; produces a number that is double the given number n
          (define (double n)
            (* n 2)))
    (build-list n double)))

; A Matrix is one of:
; – (cons Row '())
; – (cons Row Matrix)
; constraint all rows in matrix are of the same length

; A Row is one of:
; – '()
; – (cons Number Row)

; N -> Matrix
; creates a diagonal square of size n
(check-expect (identityM 1)
              (list (list 1)))
(check-expect (identityM 3)
              (list (list 1 0 0) (list 0 1 0) (list 0 0 1)))
(define (identityM n)
  (local (; N -> Row
          ; produces a Row given the position of 1
          (define (make-row pos-of-1)
            (local (; N -> N
                    ; produces 1 if p equals the position of 1,
                    ; or 0 if not
                    (define (1-or-0 p)
                      (if (= p pos-of-1) 1 0)))
              (build-list n 1-or-0))))
    (build-list n make-row)))

; N -> [List-of Number]
; tabulates a function f between n and 0 (incl.) in a list
(check-within (tabulate 0 sin)
              (list 0) 0.0001)
(check-within (tabulate 1 sin)
              (list 0.841 0) 0.001)
(check-within (tabulate 2 sin)
              (list 0.909 0.841 0) 0.001)
(check-within (tabulate 0 sqrt)
              (list 0) 0.0001)
(check-within (tabulate 1 sqrt)
              (list 1 0) 0.001)
(check-within (tabulate 2 sqrt)
              (list 1.414 1 0) 0.001)
(define (tabulate n f)
  (reverse (build-list (add1 n) f)))
