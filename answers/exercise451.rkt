#lang htdp/isl+

(define-struct table [length array])
; A Table is a structure:
;   (make-table N [N -> Number])

(define table1 (make-table 100 (lambda (i) (- i 50))))
(define table2 (make-table 100 (lambda (i) (- (* i i) 60))))

; Table N -> Number
; looks up the ith value in array of t
(define (table-ref t i)
  ((table-array t) i))

; Table -> N
; determines the smallest index for a root of the table t
; assume t is monotonically increasing
; termination (find-linear t) loops unless
; - (= (add1 i) (table-length t))
; - (< (abs (table-ref t i)) (abs (table-ref t i+1)))
(check-expect (find-linear table1) 50)
(check-expect (find-linear table2) 8)
(define (find-linear t)
  (local ((define (find-linear-helper i)
            (local ((define i+1 (add1 i)))
              (cond
                [(= i+1 (table-length t)) i]
                [(< (abs (table-ref t i))
                    (abs (table-ref t i+1))) i]
                [else (find-linear-helper i+1)]))))
    (find-linear-helper 0)))

; Table -> N
; determines the smallest index for a root of the table t
; assume t is monotonically increasing
; assume (< (table-ref t left) (table-ref t right))
; generative divides interval in half, the root index is
; in one of the two halves, picks according to (table-ref t mid)
; termination each recursive call the interval is divided in half
(check-expect (find-binary table1 0 100) 50)
(check-expect (find-binary table2 0 100) 8)
(define (find-binary t left right)
  (cond [(= (sub1 right) left)
         (if (< (abs (table-ref t left))
                (abs (table-ref t right)))
             left
             right)]
        [else
         (local ((define mid (floor (/ (+ left right) 2)))
                 (define t@mid (table-ref t mid)))
           (if (> t@mid 0)
               (find-binary t left mid)
               (find-binary t mid right)))]))

; table with 1024 slot and the root at 1024
; find-linear need 1024 calls
; find-binary need 11 calls
