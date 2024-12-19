#lang htdp/bsl+

; A List-of-numbers is one of:
; – '()
; – (cons Number List-of-numbers)

; Number List-of-numbers -> Boolean
; determines whether n occurs in a sorted list l
(check-expect (search-sorted 5 '()) #false)
(check-expect (search-sorted 5 (list 4 3 2)) #false)
(check-expect (search-sorted 5 (list 6 4 2)) #false)
(check-expect (search-sorted 5 (list 6 5 2)) #true)
(define (search-sorted n l)
  (cond
    [(empty? l) #false]
    [else (if (> n (first l))
              #false
              (or (= (first l) n)
                  (search-sorted n (rest l))))]))
