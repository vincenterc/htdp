#lang htdp/isl+

; N -> [List-of QP -> Boolean]
; produces a predicate that determines whether
; a given queen placements is a solution to an n queens puzzle
(check-expect
 ((n-queens-solution? 4) (list (make-posn 0 1)
                               (make-posn 1 3)
                               (make-posn 2 0)
                               (make-posn 3 2)))
 #true)
(check-expect
 ((n-queens-solution? 4) (list (make-posn 0 1)
                               (make-posn 1 3)
                               (make-posn 3 2)
                               (make-posn 2 0)))
 #true)
(check-expect
 ((n-queens-solution? 4) (list (make-posn 0 2)
                               (make-posn 1 0)
                               (make-posn 3 1)
                               (make-posn 2 3)))
 #true)
(check-expect
 ((n-queens-solution? 4) (list (make-posn 0 2)
                               (make-posn 1 0)
                               (make-posn 3 1)))
 #false)
(check-expect
 ((n-queens-solution? 2) (list (make-posn 0 1)
                               (make-posn 1 0)))
 #false)
(define (n-queens-solution? n)
  (lambda (loqp)
    (cond
      [(not (= (length loqp) n)) #false]
      [else
       (local ((define (solution? qps)
                 (local ((define f-qps (first qps))
                         (define r-qps (rest qps)))
                   (cond
                     [(empty? r-qps) #true]
                     [else
                      (and
                       (andmap (lambda (qp)
                                 (not (threatening? qp f-qps)))
                               r-qps)
                       (solution? r-qps))]))))
         (solution? loqp))])))

; QP QP -> Boolean
; determines whether two queens on qp1 and qp2
; would threaten each other
(define (threatening? qp1 qp2)
  (local ((define qp1x (posn-x qp1))
          (define qp1y (posn-y qp1))
          (define qp2x (posn-x qp2))
          (define qp2y (posn-y qp2)))
    (or (= qp1y qp2y)
        (= qp1x qp2x)
        (= (+ qp1x qp1y) (+ qp2x qp2y))
        (= (- qp1x qp1y) (- qp2x qp2y)))))

; [List-of X] [List-of X] -> Boolean
; determines whether set1 and set2 contain the same items
(check-expect (set=? '(0 1 2 3) '(3 1 0 2)) #true)
(check-expect (set=? '(0 1 2 3) '(3 1 0 4)) #false)
(check-expect (set=? '() '(3 1 0 4)) #false)
(define (set=? set1 set2)
  (cond [(not (= (length set1) (length set2))) #false]
        [else
         (andmap (lambda (it) (member? it set2)) set1)]))
