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

(define M1
  '((2  3  3 8)
    (2  3 -2 3)
    (4 -2  2 4)))
(define TM1
  '((2  3  3   8)
    (  -8 -4 -12)
    (     -5  -5)))

; SOE -> TM
; triangulates the given system of equations
(check-expect (triangulate M) TM)
(check-expect (triangulate M1) TM1)
(define (triangulate m)
  (cond
    [(empty? (rest m)) (list (first m))]
    [else
     (local (; SOE -> SOE
             ; rotates m0 if the first position of
             ; the first equation of m0 is 0
             (define (rotate-if-needed m0)
               (cond
                 [(not (= (first (first m0)) 0)) m0]
                 [else
                  (rotate-if-needed (append (rest m0)
                                            (list (first m0))))]))
             (define m-rotated (rotate-if-needed  m))
             (define e1 (first m-rotated)))
       (cons e1
             (triangulate
              (map (lambda (e) (subtract e e1))
                   (rest m-rotated)))))]))

; Equation Equation -> Equation
; produces an Equation with a 0 in the first position by
; subtracting a multiple of eq2 from eq1
; assume eq1 and eq2 are of equal length
(define (subtract eq1 eq2)
  (local ((define m (/ (first eq1) (first eq2))))
    (rest (map (lambda (i1 i2) (- i1 (* i2 m))) eq1 eq2))))
