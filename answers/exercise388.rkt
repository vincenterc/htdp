#lang htdp/isl+

(define-struct employee [name ssn rate])
; An Employee is a structure:
;   (make-employee String String Number)
; interpretation (make-employee n ssn r) represents an employee's name n
; ,social security number ssn and pay rate r

(define-struct work [name hours])
; A Work is a structure:
;   (make-work String Number)
; interpretation (make-work n h) represents an employee's name n and
; the number of hours h worked in a week

(define-struct wage [name amount])
; A Wage is a structure:
;   (make-wage String Number)
; interpretation (make-wage n a) represents an employee's name n and
; the weekly wage amount

; [List-of Employee] [List-of Work] -> [List-of Wage]
; produces a list of Wage for loe according to low
; assume loe and low are of equal length
; assume the corresponding list items belong to the same employee
(check-expect
 (wages*.v2 '() '())
 '())
(check-expect
 (wages*.v2 (list (make-employee "John" "123" 5.65))
            (list (make-work "John" 40)))
 (list (make-wage "John" 226)))
(check-expect
 (wages*.v2 (list (make-employee "John" "123" 5.65)
                  (make-employee "Emily" "456" 8.75))
            (list (make-work "John" 40)
                  (make-work "Emily" 30)))
 (list (make-wage "John" 226)
       (make-wage "Emily" 262.5)))
(define (wages*.v2 loe low)
  (local (; Number Number -> Number
          ; computes the weekly wage from pay-rate and hours
          (define (weekly-wage pay-rate hours)
            (* pay-rate hours)))
    (cond
      [(empty? loe) '()]
      [else
       (cons
        (make-wage (employee-name (first loe))
                   (weekly-wage (employee-rate (first loe))
                                (work-hours (first low))))
        (wages*.v2 (rest loe) (rest low)))])))
