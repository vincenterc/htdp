#lang htdp/isl+

(define-struct no-parent [])
(define NP (make-no-parent))
(define-struct child [father mother name date eyes])
; An FT (short for family tree) is one of:
; – NP
; – (make-child FT FT String N String)

; Oldest Generation:
(define Carl (make-child NP NP "Carl" 1926 "green"))
(define Bettina (make-child NP NP "Bettina" 1926 "green"))

; Middle Generation:
(define Adam (make-child Carl Bettina "Adam" 1950 "hazel"))
(define Dave (make-child Carl Bettina "Dave" 1955 "black"))
(define Eva (make-child Carl Bettina "Eva" 1965 "blue"))
(define Fred (make-child NP NP "Fred" 1966 "pink"))

; Youngest Generation:
(define Gustav (make-child Fred Eva "Gustav" 1988 "brown"))

; FT Number -> Number
; produces the average age of all child structures
; in the family tree an-ftree given the current year y
(check-expect (average-age Carl 2000) 74)
(check-expect (average-age Gustav 2000) 45.8)
(define (average-age an-ftree y)
  (/ (count-ages an-ftree y) (count-persons an-ftree)))

; FT -> N
; counts the child structures in the tree an-ftree
(check-expect (count-persons Carl) 1)
(check-expect (count-persons Gustav) 5)
(define (count-persons an-ftree)
  (cond
    [(no-parent? an-ftree) 0]
    [else (+ 1
             (count-persons (child-father an-ftree))
             (count-persons (child-mother an-ftree)))]))

; FT Number -> Number
; counts the total ages in the tree an-ftree given the current year y
(check-expect (count-ages Carl 2000) 74)
(check-expect (count-ages Gustav 2000) 229)
(define (count-ages an-ftree y)
  (cond
    [(no-parent? an-ftree) 0]
    [else (+ (- y (child-date an-ftree))
             (count-ages (child-father an-ftree) y)
             (count-ages (child-mother an-ftree) y))]))
