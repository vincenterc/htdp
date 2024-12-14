#lang htdp/bsl

; A List-of-bools is one of:
; – '()
; – (cons Boolean List-of-bools)
; interpretation a list of Boolean values

; List-of-bools -> Boolean
; determines whether all values in alob are #true
(check-expect (all-true '()) #true)
(check-expect (all-true (cons #true '())) #true)
(check-expect (all-true (cons #false '())) #false)
(check-expect (all-true (cons #true (cons #true '()))) #true)
(check-expect (all-true (cons #true (cons #false '()))) #false)
(define (all-true alob)
  (cond [(empty? alob) #true]
        [else (and (first alob)
                   (all-true (rest alob)))]))

; List-of-bools -> Boolean
; determines whether at least one item on alob is #true
(check-expect (one-true '()) #false)
(check-expect (one-true (cons #true '())) #true)
(check-expect (one-true (cons #false '())) #false)
(check-expect (one-true (cons #true (cons #true '()))) #true)
(check-expect (one-true (cons #true (cons #false '()))) #true)
(check-expect (one-true (cons #false (cons #false '()))) #false)
(define (one-true alob)
  (cond [(empty? alob) #false]
        [else (or (first alob)
                  (one-true (rest alob)))]))
