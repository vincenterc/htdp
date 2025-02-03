#lang htdp/isl+

; An SOE is a non-empty Matrix.
; constraint for (list r1 ... rn), (length ri) is (+ n 1)
; interpretation represents a system of linear equations

; An Equation is a [List-of Number].
; constraint an Equation contains at least two numbers.
; interpretation if (list a1 ... an b) is an Equation,
; a1, ..., an are the left-hand-side variable coefficients
; and b is the right-hand side

; A Solution is a [List-of Number]

; A TM is an [NEList-of Equation]
; such that the Equations are of decreasing length:
;   n + 1, n, n - 1, ..., 2.
; interpretation represents a triangular matrix

(define TM
  (list (list 2 2 3 10)
        (list   3 9 21)
        (list     1  2)))
(define S '(1 1 2))

(define TM1
  '((2  3  3   8)
    (  -8 -4 -12)
    (     -5  -5)))
(define S1 '(1 1 1))

; TM -> Solution
; solves the triangular SOE tm
(check-expect (solve TM) S)
(check-expect (solve TM1) S1)
(define (solve tm)
  (local (; Equation Solution -> Solution
          ; solves the Equation eq in n+1 variables with
          ; the solution sol for the last n variable
          (define (solve-one eq sol)
            (local ((define lhs-eq (lhs eq))
                    (define rhs-eq (rhs eq))
                    (define first-coef (first lhs-eq))
                    (define rest-coefs (rest lhs-eq)))
              (cons (/ (- rhs-eq (plug-in rest-coefs sol))
                       first-coef)
                    sol))))
    (foldr solve-one '() tm)))

; [List-of Number] Solution -> Number
; computes the value of the left-hand side of the Equation le
; with a Solution s
; assume le and s are of equal length
(define (plug-in le s)
  (foldl (lambda (i1 i2 sum) (+ (* i1 i2) sum)) 0 le s))

; Equation -> [List-of Number]
; extracts the left-hand side from a row in a matrix
(define (lhs e)
  (reverse (rest (reverse e))))

; Equation -> Number
; extracts the right-hand side from a row in a matrix
(define (rhs e)
  (first (reverse e)))
