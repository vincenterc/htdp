#lang htdp/isl+

(define THRESHOLD 5)

; [List-of Number] -> [List-of Number]
; produces a sorted version of alon
(check-expect
 (quick-sort< '(11 8 14 7))
 '(7 8 11 14))
(check-expect
 (quick-sort< '(5 11 8 5 14 7 5))
 '(5 5 5 7 8 11 14))
(define (quick-sort< alon)
  (cond
    [(empty? alon) '()]
    [(empty? (rest alon)) alon]
    [else (local ((define pivot (first alon))
                  (define others (rest alon)))
            (append (quick-sort< (smaller-and-equals others pivot))
                    (list pivot)
                    (quick-sort< (largers others pivot))))]))

; [List-of Number] Number -> [List-of Number]
(define (largers alon n)
  (filter (lambda (n0) (> n0 n)) alon))

; [List-of Number] Number -> [List-of Number]
(define (smaller-and-equals alon n)
  (filter (lambda (n0) (<= n0 n)) alon))
