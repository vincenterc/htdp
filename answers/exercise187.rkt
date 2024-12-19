#lang htdp/bsl+

(define-struct gp [name score])
; A GamePlayer is a structure:
;    (make-gp String Number)
; interpretation (make-gp p s) represents player p who
; scored a maximum of s points

; List-of-gps is one of:
; - '()
; - (cons GamePlayer List-of-gps)

(define gp1 (make-gp "a" 10))
(define gp2 (make-gp "b" 8))
(define gp3 (make-gp "c" 5))

; List-of-gps -> List-of-gps
; sorts a list of game players by score
(check-expect (sort-gp '()) '())
(check-expect (sort-gp (list gp1)) (list gp1))
(check-expect (sort-gp (list gp1 gp2 gp3))
              (list gp1 gp2 gp3))
(check-expect (sort-gp (list gp2 gp3 gp1))
              (list gp1 gp2 gp3))
(define (sort-gp l)
  (cond [(empty? l) '()]
        [else (insert-gp (first l) (sort-gp (rest l)))]))

; GamePlayer List-of-gps -> List-of-gps
; inserts gp into the sorted list of game player l
(check-expect (insert-gp gp2 (list gp1 gp3))
              (list gp1 gp2 gp3))
(define (insert-gp gp l)
  (cond [(empty? l) (cons gp '())]
        [else (if (gp>? gp (first l))
                  (cons gp l)
                  (cons (first l) (insert-gp gp (rest l))))]))

; GamePlayer GamePlayer -> Boolean
; compares the scores of two game players for greater-than
(check-expect (gp>? gp1 gp2) #true)
(check-expect (gp>? gp3 gp2) #false)
(define (gp>? gp1 gp2)
  (> (gp-score gp1) (gp-score gp2)))