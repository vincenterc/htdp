#lang htdp/isl+

(define labels '("Name" "Present"))

; Row [List-of Label] -> Row
; retains those cells whose corresponding element
; in names is also in labels
; assume row and names are of equal length
(check-expect
 (row-filter '("Alice" 35 #true)
             '("Name" "Age" "Present"))
 '("Alice" #true))
(check-expect
 (row-filter '("Bob"   25 #false)
             '("Name" "Age" "Present"))
 '("Bob"   #false))
(define (row-filter row names)
  (cond
    [(empty? names) '()]
    [else
     (if (member? (first names) labels)
         (cons (first row)
               (row-filter (rest row) (rest names)))
         (row-filter (rest row) (rest names)))]))
