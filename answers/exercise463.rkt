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

(define M2
  '((2 2 3 10)
    (0 3 9 21)
    (0 0 1  2)))
(define S '(1 1 2))
(check-satisfied M2 (lambda (m) (check-solution m S)))

; SOE Solution -> Boolean
; checks if the Solution s is correct for soe
(define (check-solution soe s)
  (andmap (lambda (e) (= (plug-in (lhs e) s) (rhs e))) soe))

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
