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

; Equation Equation -> Equation
; produces an Equation with a 0 in the first position by
; subtracting a multiple of eq2 from eq1
; assume eq1 and eq2 are of equal length
(check-expect
 (subtract '(2 5 12 31) '(2 2 3 10)) '(3 9 21))
(check-expect
 (subtract '(4 1 -2  1) '(2 2 3 10)) '(-3 -8 -19))
(define (subtract eq1 eq2)
  (local ((define m (/ (first eq1) (first eq2))))
    (rest (map (lambda (i1 i2) (- i1 (* i2 m))) eq1 eq2))))
