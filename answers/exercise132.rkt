#lang htdp/bsl

; A List-of-names is one of:
; – '()
; – (cons String List-of-names)
; interpretation a list of invitees, by last name

; List-of-names -> Boolean
; determines whether "Flatt" is on alon
(check-expect (contains-flatt? '()) #false)
(check-expect (contains-flatt? (cons "Find" '()))
              #false)
(check-expect (contains-flatt? (cons "Flatt" '()))
              #true)
(check-expect
 (contains-flatt? (cons "X" (cons "Y"  (cons "Z" '()))))
 #false)
(check-expect
 (contains-flatt? (cons "A" (cons "Flatt" (cons "C" '()))))
 #true)
(define (contains-flatt? alon)
  (cond
    [(empty? alon) #false]
    [(cons? alon)
     (or (string=? (first alon) "Flatt")
         (contains-flatt? (rest alon)))]))

; (contains-flatt?
;  (cons
;   "Fagan"
;   (cons
;    "Findler"
;    (cons
;     "Fisler"
;     (cons
;      "Flanagan"
;      (cons
;       "Flatt"
;       (cons
;        "Felleisen"
;        (cons "Friedman" '()))))))))
