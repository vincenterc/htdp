#lang htdp/bsl

(define-struct work [employee rate hours])
; A (piece of) Work is a structure:
;   (make-work String Number Number)
; interpretation (make-work n r h) combines the name
; with the pay rate r and the number of hours h

; Low (short for list of works) is one of:
; – '()
; – (cons Work Low)
; interpretation an instance of Low represents the
; hours worked for a number of employees

(define-struct paycheck [employee amount])
; A Paycheck is a structure:
;   (make-paycheck String Number)
; interpretation (make-work n a) combines the name n
; with amount a

; Lop (short for list of paychecks) is one of:
; – '()
; – (cons Paycheck Lop)
; interpretation an instance of Lop represents the
; wages for a number of employees

; Low -> Lop
; computes the weekly wages for all weekly work records
(check-expect
 (wage*.v3 (cons (make-work "Robby" 11.95 39) '()))
 (cons (make-paycheck "Robby" (* 11.95 39)) '()))
(define (wage*.v3 an-low)
  (cond
    [(empty? an-low) '()]
    [(cons? an-low)
     (cons (create-paycheck (first an-low))
           (wage*.v3 (rest an-low)))]))

; Work -> Paycheck
; creates a paycheck given w
(define (create-paycheck w)
  (make-paycheck (work-employee w)
                 (wage.v2 w)))

; Work -> Number
; computes the wage for the given work record w
(define (wage.v2 w)
  (* (work-rate w) (work-hours w)))

(define-struct employee [name number])
; A Employee is a structure:
;   (make-employee String Number)
; interpretation (make-employee n num) represents
; an employee named n with the number num

(define-struct work.v2 [employee rate hours])
; A (piece of) Work.v2 is a structure:
;   (make-work Employee Number Number)
; interpretation (make-work e r h) combines the employee information e
; with the pay rate r and the number of hours h

; Low.v2 (short for list of works) is one of:
; – '()
; – (cons Work.v2 Low)
; interpretation an instance of Low.v2 represents the
; hours worked for a number of employees

(define-struct paycheck.v2 [employee amount])
; A Paycheck.v2 is a structure:
;   (make-paycheck Employee Number)
; interpretation (make-work e a) combines the employee information e
; with amount a

; Lop.v2 (short for list of paychecks) is one of:
; – '()
; – (cons Paycheck.v2 Lop)
; interpretation an instance of Lop.v2 represents the
; wages for a number of employees

; Low.v2 -> Lop.v2
; computes the weekly wages for all weekly work records
(check-expect
 (wage*.v4 (cons (make-work.v2 (make-employee "Robby" 12345) 11.95 39) '()))
 (cons (make-paycheck.v2 (make-employee "Robby" 12345) (* 11.95 39)) '()))
(define (wage*.v4 an-low)
  (cond
    [(empty? an-low) '()]
    [(cons? an-low)
     (cons (create-paycheck.v2 (first an-low))
           (wage*.v4 (rest an-low)))]))

; Work.v2 -> Paycheck.v2
; creates a paycheck given w
(define (create-paycheck.v2 w)
  (make-paycheck.v2 (work.v2-employee w)
                    (wage.v3 w)))

; Work.v2 -> Number
; computes the wage for the given work record w
(define (wage.v3 w)
  (* (work.v2-rate w) (work.v2-hours w)))
