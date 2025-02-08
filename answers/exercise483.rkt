#lang htdp/isl+

(require 2htdp/abstraction)
(require 2htdp/image)

; A QP is a structure:
;   (make-posn CI CI)
; A CI is an N in [0,QUEENS).
; interpretation (make-posn r c) denotes the square at
; the r-th row and c-th column

; A Board is a [List-of QP]
; interpretation a list of QPs
; where a queen can still be safely placed

; N -> [Maybe [List-of QP]]
; finds a solution to the n queens problem
(check-expect (n-queens 2) #false)
(check-expect (n-queens 3) #false)
(check-satisfied (n-queens 4) (n-queens-solution? 4))
(define (n-queens n)
  (place-queens (board0 n) n))

; Board N -> [Maybe [List-of QP]]
; places n queens on board; otherwise, returns #false
(define (place-queens a-board n)
  (cond
    [(= n 0) '()]
    [else
     (local (; [List-of QP] -> [Maybe [List-of QP]]
             (define (place-queens/list loqp)
               (cond
                 [(empty? loqp) #false]
                 [else
                  (local ((define first-qp (first loqp))
                          ; [Maybe [List-of QP]]
                          (define candidate
                            (place-queens
                             (add-queen a-board first-qp)
                             (sub1 n))))
                    (cond [(boolean? candidate)
                           (place-queens/list (rest loqp))]
                          [else (cons first-qp candidate)]))])))
       (place-queens/list a-board))]))

; N -> Board
; creates the initial n by n board
(define (board0 n)
  (for*/list ([i n] [j n]) (make-posn i j)))

; Board QP -> Board
; places a queen at qp on a-board
(check-expect (add-queen (board0 2) (make-posn 0 0)) '())
(check-expect (add-queen (board0 3) (make-posn 0 0))
              (list (make-posn 1 2) (make-posn 2 1)))
(define (add-queen a-board qp0)
  (filter (lambda (qp) (not (threatening? qp qp0))) a-board))

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

; N -> [List-of QP -> Boolean]
; produces a predicate that determines whether
; a given queen placements is a solution to an n queens puzzle
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

(define QUEEN (text "Q" 16 "black"))

(define CELL-SIZE 20)
(define HALF-CELL-SIZE (/ CELL-SIZE 2))
(define CELL (square CELL-SIZE "outline" "black"))

; N [List-of QP] Image -> Image
; produces an image of an n by n chess board
; with img placed according to loqp
(define (render-queens n loqp img)
  (local ((define row
            (foldr beside empty-image
                   (build-list n (lambda (n) CELL))))
          (define board
            (foldr above empty-image
                   (build-list n (lambda (n) row))))
          (define (qp->pos qp)
            (make-posn (+ (* (posn-x qp) CELL-SIZE)
                          HALF-CELL-SIZE)
                       (+ (* (posn-y qp) CELL-SIZE)
                          HALF-CELL-SIZE)))
          (define lop (map qp->pos loqp)))
    (foldr (lambda (p scene)
             (place-image QUEEN (posn-x p) (posn-y p) scene))
           board lop)))

; (render-queens 4 (n-queens 4) QUEEN)
