#lang htdp/isl+

; A Son is one of:
; – empty
; – (cons Number Son.R)
; Constraint If s is a Son
; no number occurs twice in s

; Son Son -> Son
; produces Son that contains the elements of s1 and s2
(check-expect (union '() '(1 2)) '(1 2))
(check-expect (union '(2 3) '(1 2)) '(3 1 2))
(check-expect (union '(3 4) '(1 2)) '(3 4 1 2))
(define (union s1 s2)
  (cond [(empty? s1) s2]
        [else (if (member? (first s1) s2)
                  (union (rest s1) s2)
                  (cons (first s1) (union (rest s1) s2)))]))

; Son Son -> Son
; produces Son in which elements occur in both s1 and s2
(check-expect (intersect '() '(1 2)) '())
(check-expect (intersect '(2 3) '(1 2)) '(2))
(check-expect (intersect '(3 4) '(1 2)) '())
(define (intersect s1 s2)
  (cond [(empty? s1) '()]
        [else (if (member? (first s1) s2)
                  (cons (first s1) (intersect (rest s1) s2))
                  (intersect (rest s1) s2))]))
