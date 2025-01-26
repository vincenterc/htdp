#lang htdp/isl+

(define-struct employee [name number rate])
; An Employee is a structure:
;   (make-employee String String Number)
; interpretation (make-employee n num r) represents an employee's name n
; ,number num and pay rate r

(define-struct card [number hours])
; A Card is a structure:
;   (make-card String Number)
; interpretation (make-card n h) represents an employee number n and
; the number of hours h worked per week

(define-struct wage [name amount])
; A Wage is a structure:
;   (make-wage String Number)
; interpretation (make-wage n a) represents an employee's name n and
; the weekly wage amount

(define CARD-NOT-FOUND "Time card not found")
(define EMPLOYEE-NOT-FOUND "Employee record not found")

; [List-of Employee] [List-of Card] -> [List-of Wage]
; produces a list of Wage for loe according to loc
; signals an error if cannot find a time card for an employee record
; or vice versa
(check-expect
 (wages*.v3 '() '())
 '())
(check-error
 (wages*.v3 '() (list (make-card "123" 40)))
 EMPLOYEE-NOT-FOUND)
(check-error
 (wages*.v3 (list (make-employee "John" "123" 5.65)) '())
 CARD-NOT-FOUND)
(check-expect
 (wages*.v3 (list (make-employee "John" "123" 5.65))
            (list (make-card "123" 40)))
 (list (make-wage "John" 226)))
(check-expect
 (wages*.v3 (list (make-employee "John" "123" 5.65)
                  (make-employee "Emily" "456" 8.75))
            (list (make-card "123" 40)
                  (make-card "456" 30)))
 (list (make-wage "John" 226)
       (make-wage "Emily" 262.5)))
(check-error
 (wages*.v3 (list (make-employee "John" "123" 5.65)
                  (make-employee "Emily" "456" 8.75))
            (list (make-card "223" 40)
                  (make-card "456" 30)))
 CARD-NOT-FOUND)
(define (wages*.v3 loe loc)
  (cond
    [(cons? loe)
     (local (; Number Number -> Number
             ; computes the weekly wage from rate and hours
             (define (weekly-wage rate hours)
               (* rate hours))
             ; Number -> [Maybe Card]
             ; produces a card with number num from loc
             ; produces #false if such card does not exist
             (define (find-card num cards)
               (cond [(empty? cards) #false]
                     [else (if (string=? (card-number (first cards)) num)
                               (first cards)
                               (find-card num (rest cards)))]))
             (define employee (first loe))
             (define employee-num (employee-number employee))
             (define card (find-card employee-num loc)))
       (if (card? card)
           (cons (make-wage
                  (employee-name employee)
                  (weekly-wage (employee-rate employee)
                               (card-hours card)))
                 (wages*.v3 (rest loe) (remove card loc)))
           (error CARD-NOT-FOUND)))]
    [(empty? loc) '()]
    [(cons? loc) (error EMPLOYEE-NOT-FOUND)]))
