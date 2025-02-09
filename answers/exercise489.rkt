#lang htdp/isl+

; Number [List-of Number] -> [List-of Number]
; adds n to each number on l
(check-expect (cons 50 (add-to-each 50 '(40 110 140 170)))
              '(50 90 160 190 220))
(define (add-to-each n l)
  (map (lambda (m) (+ m n)) l))
