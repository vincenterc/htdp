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

(define M
  (list (list 2 2  3 10)
        (list 2 5 12 31)
        (list 4 1 -2  1)))
(define TM
  (list (list 2 2 3 10)
        (list   3 9 21)
        (list     1  2)))

; SOE -> TM
; triangulates the given system of equations
(check-expect (triangulate M) TM)
(define (triangulate m)
  (cond
    [(empty? (rest m)) (list (first m))]
    [else
     (local ((define e1 (first m)))
       (cons e1
             (triangulate
              (map (lambda (e) (subtract e e1)) (rest m)))))]))

; Equation Equation -> Equation
; produces an Equation with a 0 in the first position by
; subtracting a multiple of eq2 from eq1
; assume eq1 and eq2 are of equal length
(define (subtract eq1 eq2)
  (local ((define m (/ (first eq1) (first eq2))))
    (rest (map (lambda (i1 i2) (- i1 (* i2 m))) eq1 eq2))))
