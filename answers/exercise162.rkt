#lang htdp/bsl

(define WAGE-PER-HOUR 14)

; Number -> Number
; computes the wage for h hours of work
(define (wage h)
  (* WAGE-PER-HOUR h))

; A List-of-numbers is one of:
; – '()
; – (cons Number List-of-numbers)

; List-of-numbers -> List-of-numbers
; computes the weekly wages for the weekly hours
(check-expect (wage* '())
              '())
(check-expect (wage* (cons 28 '()))
              (cons (* WAGE-PER-HOUR 28) '()))
(check-expect (wage* (cons 4 (cons 2 '())))
              (cons (* WAGE-PER-HOUR 4) (cons (* WAGE-PER-HOUR 2) '())))
(check-error (wage* (cons 110 '())))
(define (wage* whrs)
  (cond
    [(empty? whrs) '()]
    [else (if (> (first whrs) 100)
              (error "work more than 100 hours per week")
              (cons (wage (first whrs)) (wage* (rest whrs))))]))
