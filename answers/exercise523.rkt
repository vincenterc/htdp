#lang htdp/isl+

(define-struct ps [left right boat states])
; A PuzzleState is a structure:
;   (make-ps Bank Bank Boat [List-of PuzzleState])
; accumulator states contains the sequence of states traversed to reach
; the current state
(define-struct bank [miss cann])
; A Bank is a structure:
;   (make-bank N N)
; interpretation (make-bank m c) represents m missionaries
; and c cannibals
; A Boat is one of:
; - 'left
; - 'right

(define initial-puzzle
  (make-ps (make-bank 3 3) (make-bank 0 0) 'left '()))
(define final-puzzle
  (make-ps (make-bank 0 0) (make-bank 3 3) 'right '()))
(define intermediate-puzzle
  (make-ps (make-bank 1 1) (make-bank 2 2) 'left '()))
(define intermediate-puzzle-1
  (make-ps (make-bank 3 2) (make-bank 0 1) 'right (list initial-puzzle)))
(define intermediate-puzzle-2
  (make-ps (make-bank 3 1) (make-bank 0 2) 'right (list initial-puzzle)))
(define intermediate-puzzle-3
  (make-ps (make-bank 2 2) (make-bank 1 1) 'right (list initial-puzzle)))

; [List-of PuzzleState] -> [List-of PuzzleState]
; produces a list of states that a boat ride can reach from
; the given states lops
(check-expect
 (create-next-states (list initial-puzzle))
 (list intermediate-puzzle-1
       intermediate-puzzle-2
       intermediate-puzzle-3))
(check-expect
 (create-next-states
  (list intermediate-puzzle-1
        intermediate-puzzle-2
        intermediate-puzzle-3))
 (list (make-ps (make-bank 3 2) (make-bank 0 1) 'left
                (list intermediate-puzzle-3 initial-puzzle))
       (make-ps (make-bank 3 2) (make-bank 0 1) 'left
                (list intermediate-puzzle-2 initial-puzzle))))
(define (create-next-states lops0)
  (local (; [List-of PuzzleState] [List-of PuzzleState] ->
          ; [List-of PuzzleState]
          ; accumulator a is a list of states that can be reached from
          ; states that lops lacks from lops0
          (define (create-next-states/a lops a)
            (cond
              [(empty? lops) a]
              [else (create-next-states/a
                     (rest lops)
                     (append
                      (create-next-states/one (first lops)) a))])))
    (create-next-states/a lops0 '())))

; PuzzleState -> [List-of PuzzleState]
; produces the next possible states given the state ps
(check-expect
 (create-next-states/one initial-puzzle)
 (list intermediate-puzzle-1
       intermediate-puzzle-2
       intermediate-puzzle-3))
(check-expect
 (create-next-states/one intermediate-puzzle-3)
 (list (make-ps (make-bank 3 2) (make-bank 0 1) 'left
                (list intermediate-puzzle-3
                      initial-puzzle))))
(define (create-next-states/one ps)
  (local ((define ss (ps-states ps))
          (define b (ps-boat ps))
          (define l (ps-left ps))
          (define l-m (bank-miss l))
          (define l-c (bank-cann l))
          (define r (ps-right ps))
          (define r-m (bank-miss r))
          (define r-c (bank-cann r))
          ; N N Boat -> PuzzleState
          ; produces the next state if m missionaries and
          ; c cannibals traverse to the dir bank
          (define (traverse m c dir)
            (if (symbol=? dir 'left)
                (make-ps (make-bank (+ l-m m) (+ l-c c))
                         (make-bank (- r-m m) (- r-c c))
                         'left
                         (cons ps ss))
                (make-ps (make-bank (- l-m m) (- l-c c))
                         (make-bank (+ r-m m) (+ r-c c))
                         'right
                         (cons ps ss))))
          ; Boat -> [List-of PuzzleState]
          (define (make-next-states dir)
            (list (traverse 0 1 dir)
                  (traverse 0 2 dir)
                  (traverse 1 1 dir)
                  (traverse 2 0 dir)
                  (traverse 1 0 dir)))
          (define next-states
            (if (symbol=? b 'left)
                (make-next-states 'right)
                (make-next-states 'left))))
    (filter (lambda (ns)
              (and (valid? ns)
                   (not (member?/ps ns ss))))
            next-states)))

; PuzzleState -> Boolean
; determines whether the given state ps is valid
(check-expect
 (valid? (make-ps (make-bank 3 3) (make-bank 0 0) 'left '()))
 #true)
(check-expect
 (valid? (make-ps (make-bank 3 2) (make-bank 0 1) 'left '()))
 #true)
(check-expect
 (valid? (make-ps (make-bank 1 3) (make-bank 2 0) 'right '()))
 #false)
(check-expect
 (valid? (make-ps (make-bank 2 1) (make-bank 1 2) 'right '()))
 #false)
(check-expect
 (valid? (make-ps (make-bank 3 4) (make-bank 0 -1) 'left '()))
 #false)
(define (valid? ps)
  (local ((define l-b (ps-left ps))
          (define l-b-m (bank-miss l-b))
          (define l-b-c (bank-cann l-b))
          (define r-b (ps-right ps))
          (define r-b-m (bank-miss r-b))
          (define r-b-c (bank-cann r-b)))
    (and (<= 0 l-b-m 3)
         (<= 0 l-b-c 3)
         (or (= l-b-m 0) (>= l-b-m l-b-c))
         (<= 0 r-b-m 3)
         (<= 0 r-b-c 3)
         (or (= r-b-m 0) (>= r-b-m r-b-c)))))

; PuzzleState [List-of PuzzleState] -> Boolean
; checks whether ps is in lops
; (only checks the equality of the left, right and boat fields)
(check-expect
 (member?/ps intermediate-puzzle-1
             (list intermediate-puzzle-1
                   intermediate-puzzle-2
                   intermediate-puzzle-3))
 #true)
(check-expect
 (member?/ps intermediate-puzzle
             (list intermediate-puzzle-1
                   intermediate-puzzle-2
                   intermediate-puzzle-3))
 #false)
(define (member?/ps ps lops)
  (cond
    [(empty? lops) #false]
    [else
     (local ((define ps-l (ps-left ps))
             (define ps-r (ps-right ps))
             (define ps-b (ps-boat ps))
             (define f-ps (first lops))
             (define f-ps-l (ps-left f-ps))
             (define f-ps-r (ps-right f-ps))
             (define f-ps-b (ps-boat f-ps)))
       (or (and (equal? ps-l f-ps-l)
                (equal? ps-r f-ps-r)
                (eq? ps-b f-ps-b))
           (member?/ps ps (rest lops))))]))
