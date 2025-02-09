#lang htdp/isl+

; [List-of X] -> [List-of X]
; constructs the reverse of alox
(check-expect (invert '(a b c)) '(c b a))
(define (invert alox)
  (cond
    [(empty? alox) '()]
    [else
     (add-as-last (first alox) (invert (rest alox)))]))

; X [List-of X] -> [List-of X]
; adds an-x to the end of alox
(check-expect (add-as-last 'a '(c b)) '(c b a))
(define (add-as-last an-x alox)
  (cond
    [(empty? alox) (list an-x)]
    [else
     (cons (first alox) (add-as-last an-x (rest alox)))]))

; n items require n recursions of invert
; and n calls of add-as-last.
; Each add-as-last call requires n recursions of add-as-last
; if the size of the list is n.
; The total recursive calls of add-as-last is n^2 / 2 - n / 2
; So the total steps is n^2 / 2 + n / 2 (n + n^2 / 2 - n / 2).
; That is O(n^2)
