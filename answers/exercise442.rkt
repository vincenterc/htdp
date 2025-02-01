#lang htdp/isl+

(define THRESHOLD 50)

; [List-of Number] -> [List-of Number]
; produces a sorted version of alon
(check-expect
 (clever-sort< '(11 8 14 7))
 '(7 8 11 14))
(check-expect
 (clever-sort< '(5 11 8 5 14 7 5))
 '(5 5 5 7 8 11 14))
(define (clever-sort< alon)
  (cond
    [(<= (length alon) THRESHOLD) (sort< alon)]
    [else (quick-sort< alon)]))

; (test-sort clever-sort< 300 50)
; cpu time: 0 real time: 0 gc time: 0
; cpu time: 0 real time: 0 gc time: 0
; cpu time: 0 real time: 0 gc time: 0
; cpu time: 0 real time: 1 gc time: 0
; cpu time: 0 real time: 1 gc time: 0
; cpu time: 0 real time: 1 gc time: 0
; cpu time: 15 real time: 2 gc time: 0

; [[List-of Number] -> [List-of Number]] N N ->
; [List-of [List-of Number]]
(define (test-sort fn max step)
  (map (lambda (n) (time (fn (create-tests n max))))
       (build-list (+ (/ max step) 1) (lambda (n) (* n step)))))

; (test-sort sort< 300 50)
; 0   cpu time: 0 real time: 0 gc time: 0
; 50  cpu time: 0 real time: 0 gc time: 0
; 100 cpu time: 0 real time: 2 gc time: 0
; 150 cpu time: 15 real time: 4 gc time: 0
; 200 cpu time: 0 real time: 7 gc time: 0
; 250 cpu time: 31 real time: 29 gc time: 0
; 300 cpu time: 15 real time: 14 gc time: 0

; (test-sort quick-sort< 300 50)
; 0   cpu time: 0 real time: 0 gc time: 0
; 50  cpu time: 0 real time: 0 gc time: 0
; 100 cpu time: 0 real time: 0 gc time: 0
; 150 cpu time: 15 real time: 1 gc time: 0
; 200 cpu time: 0 real time: 1 gc time: 0
; 250 cpu time: 0 real time: 2 gc time: 0
; 300 cpu time: 0 real time: 4 gc time: 0

; N -> [List-of number]
; creates a list of n numbers in the range [0, max)
(check-expect (length (create-tests 0 10)) 0)
(check-expect (length (create-tests 1 10)) 1)
(check-expect (length (create-tests 10 100)) 10)
(define (create-tests n max)
  (cond [(zero? n) '()]
        [else (cons (random max)
                    (create-tests (sub1 n) max))]))

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

; List-of-numbers -> List-of-numbers
; produces a sorted version of l
(check-expect
 (sort< '(11 8 14 7))
 '(7 8 11 14))
(check-expect
 (quick-sort< '(5 11 8 5 14 7 5))
 '(5 5 5 7 8 11 14))
(define (sort< l)
  (cond
    [(empty? l) '()]
    [(cons? l) (insert (first l) (sort< (rest l)))]))

; Number List-of-numbers -> List-of-numbers
; inserts n into the sorted list of numbers l
(define (insert n l)
  (cond
    [(empty? l) (cons n '())]
    [else (if (<= n (first l))
              (cons n l)
              (cons (first l) (insert n (rest l))))]))
