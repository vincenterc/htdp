#lang htdp/bsl

; An NEList-of-Booleans is one of:
; – (cons Boolean '())
; – (cons Boolean NEList-of-Booleans)
; interpretation non-empty lists of Boolean values

; NEList-of-Booleans -> Boolean
; determines whether all values in ne-l are #true
(check-expect (all-true (cons #true '())) #true)
(check-expect (all-true (cons #false '())) #false)
(check-expect (all-true (cons #true (cons #true '()))) #true)
(check-expect (all-true (cons #true (cons #false '()))) #false)
(define (all-true ne-l)
  (cond [(empty? (rest ne-l)) (first ne-l)]
        [else (and (first ne-l)
                   (all-true (rest ne-l)))]))

; NEList-of-Booleans -> Boolean
; determines whether at least one item on ne-l is #true
(check-expect (one-true (cons #true '())) #true)
(check-expect (one-true (cons #false '())) #false)
(check-expect (one-true (cons #true (cons #true '()))) #true)
(check-expect (one-true (cons #true (cons #false '()))) #true)
(check-expect (one-true (cons #false (cons #false '()))) #false)
(define (one-true ne-l)
  (cond [(empty? (rest ne-l)) (first ne-l)]
        [else (or (first ne-l)
                  (one-true (rest ne-l)))]))
