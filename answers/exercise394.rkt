#lang htdp/isl+

; [List-of Number] [Lis-of Number] -> [List-of Number]
; produces an ascending sorted list of number
; given two ascending sorted list of numbers lon1 and lon2
; a number occurs in the outputs as many times as it occurs on the
; lon1 and lon2 together
(check-expect (merge '() '(1 2)) '(1 2))
(check-expect (merge '(3 4) '(1 2)) '(1 2 3 4))
(check-expect (merge '(2 3) '(1 2)) '(1 2 2 3))
(define (merge lon1 lon2)
  (local ((define (insert n lon)
            (cond [(empty? lon) (list n)]
                  [else (if (<= n (first lon))
                            (cons n lon)
                            (cons (first lon) (insert n (rest lon))))])))
    (cond [(empty? lon1) lon2]
          [else (merge (rest lon1)
                       (insert (first lon1) lon2))])))
; Alternative:
; (check-expect (merge '() '()) '())
; (check-expect (merge '() '(1 2)) '(1 2))
; (check-expect (merge '(3 4) '()) '(3 4))
; (check-expect (merge '(3 4) '(1 2)) '(1 2 3 4))
; (check-expect (merge '(2 3) '(1 2)) '(1 2 2 3))
; (define (merge lon1 lon2)
;   (cond [(empty? lon1) lon2]
;         [(empty? lon2) lon1]
;         [(cons? lon2)
;          (if (<= (first lon1) (first lon2))
;              (cons (first lon1) (merge (rest lon1) lon2))
;              (cons (first lon2) (merge lon1 (rest lon2))))]))
