#lang htdp/isl+

(require 2htdp/image)
(require 2htdp/universe)

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

; PuzzleState -> [Maybe [List-of PuzzleState]]
; is the final state reachable from state0
; generative creates a tree of possible boat rides
; termination ???
(define (solve state0)
  (local (; [List-of PuzzleState] -> PuzzleState
          ; generative generates the successors of los
          (define (solve* los)
            (cond
              [(ormap final? los)
               (reverse
                (cons final-puzzle
                      (ps-states (first (filter final? los)))))]
              [else
               (solve* (create-next-states los))])))
    (solve* (list state0))))

; PuzzleState -> Boolean
; determines whether all people are on the right river bank
; in the given state ps
(define (final? ps)
  (local (; Bank
          (define r-bank (ps-right ps)))
    (and (= (bank-miss r-bank) 3)
         (= (bank-cann r-bank) 3))))

; [List-of PuzzleState] -> [List-of PuzzleState]
; produces a list of states that a boat ride can reach from
; the given states lops
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

(define RADIUS 5)
(define DIAMETER (* RADIUS 2))
(define MISS (circle RADIUS "solid" "black"))
(define CANN (circle RADIUS "outline" "black"))
(define BOARD 2)
(define BANK-WIDTH (+ (* DIAMETER 2) (* BOARD 3)))
(define BANK-HEIGHT (+ (* DIAMETER 3) (* BOARD 4)))
(define BANK
  (rectangle BANK-WIDTH BANK-HEIGHT "outline" "black"))
(define BOAT
  (overlay/offset
   (rhombus 6.5 100 "solid" "black")
   0 RADIUS
   (rectangle DIAMETER RADIUS "solid" "black")))
(define RIVER
  (rectangle (* BANK-WIDTH 2) BANK-HEIGHT "outline" "black"))

; PuzzleState -> Image
; maps the state ps to an image
(define (render-mc ps)
  (beside (render-bank (ps-left ps))
          (render-river (ps-boat ps))
          (render-bank (ps-right ps))))

; Bank -> Image
; produces a bank image given the bank state b
(define (render-bank b)
  (local ((define m (bank-miss b))
          (define c (bank-cann b))
          (define radius+board (+ RADIUS BOARD))
          (define diameter+board (+ DIAMETER BOARD))
          ; N Image Number Image -> Image
          (define (render n img x back)
            (foldl (lambda (i acc)
                     (place-image
                      img
                      x
                      (+ radius+board (* diameter+board i))
                      acc))
                   back
                   (build-list n identity))))
    (render c CANN (+ radius+board diameter+board)
            (render m MISS radius+board BANK))))

; Boat -> Image
; produces a river image given the boat state b
(define (render-river b)
  (local ((define shift (- BANK-WIDTH RADIUS BOARD)))
    (overlay/offset
     BOAT
     (if (symbol=? b 'left) shift (- shift))
     0
     RIVER)))

; (run-movie 1 (map render-mc (solve initial-puzzle)))