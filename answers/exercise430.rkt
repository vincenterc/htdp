#lang htdp/isl+

(define THRESHOLD 5)

; [List-of Number] [Number Number -> Boolean] -> [List-of Number]
; produces a sorted version of alon given a comparison cmp
(check-expect
 (quick-sort '(11 8 14 7) <)
 '(7 8 11 14))
(check-expect
 (quick-sort '(5 11 8 5 14 7 5) <)
 '(5 5 5 7 8 11 14))
(check-expect
 (quick-sort '(11 8 14 7) >)
 '(14 11 8 7))
(check-expect
 (quick-sort '(5 11 8 5 14 7 5) >)
 '(14 11 8 7 5 5 5))
(define (quick-sort alon cmp)
  (local ((define (group1 lon n)
            (filter (lambda (n0) (cmp n0 n)) lon))
          (define (group2 lon n)
            (filter (lambda (n0) (not (cmp n0 n))) lon)))
    (cond
      [(empty? alon) '()]
      [(empty? (rest alon)) alon]
      [else (local ((define pivot (first alon))
                    (define others (rest alon)))
              (append (quick-sort (group1 others pivot) cmp)
                      (list pivot)
                      (quick-sort (group2 others pivot) cmp)))])))
