#lang htdp/bsl+

(require 2htdp/image)
(require 2htdp/universe)

(define WIDTH 10) ; # of blocks, horizontally
(define HEIGHT 10) ; # of blocks, vertically

(define SIZE 10) ; blocks are squares
(define HALF-SIZE (/ SIZE 2))
(define BLOCK ; red squares with black rims
  (overlay
   (square (- SIZE 1) "solid" "red")
   (square SIZE "outline" "black")))
(define BLOCK-REST
  (square SIZE "solid" "grey"))

(define SCENE-WIDTH (* WIDTH SIZE))
(define SCENE-HEIGHT (* HEIGHT SIZE))
(define SCENE (empty-scene SCENE-WIDTH SCENE-HEIGHT))

(define FONT-SIZE 10)
(define FONT-COLOR "black")

; A Posns is one of:
; - '()
; - (cons Posn Lop)

(define LEFT "left")
(define RIGHT "right")
(define DOWN "down")
; A Direction is on of:
; - LEFT
; - RIGHT
; - DOWN

(define-struct tetris [block landscape])
(define-struct block [x y])
; A Tetris is a structure:
;   (make-tetris Block Landscape)
; A Landscape is one of:
; – '()
; – (cons Block Landscape)
; A Block is a structure:
;   (make-block N N)
; interpretations
; (make-block x y) depicts a block whose left, top
; corner is (* x SIZE) pixels from the left and
; (* y SIZE) pixels from the top;
; (make-tetris b0 (list b1 b2 ...)) means b0 is the
; dropping block, while b1, b2, and ... are resting

(define landscape0 (list (make-block 6 (- HEIGHT 3))
                         (make-block 6 (- HEIGHT 2))
                         (make-block 6 (- HEIGHT 1))
                         (make-block 5 (- HEIGHT 1))))
(define landscape-end (list (make-block 6 0)
                            (make-block 6 1)
                            (make-block 6 2)
                            (make-block 6 3)
                            (make-block 6 4)
                            (make-block 6 5)
                            (make-block 6 6)
                            (make-block 6 (- HEIGHT 3))
                            (make-block 6 (- HEIGHT 2))
                            (make-block 6 (- HEIGHT 1))))
(define block0 (make-block 4 0))
(define block-dropping (make-block 4 4))
(define block-dropping-moved-down (make-block 4 5))
(define block-dropping-moved-left (make-block 3 4))
(define block-dropping-moved-right (make-block 5 4))
(define block-landed (make-block 4 (- HEIGHT 1)))
(define block-landed-moved-down (make-block 4 HEIGHT))
(define block-on-block (make-block 5 (- HEIGHT 2)))
(define block-on-block-moved-down (make-block 5 (- HEIGHT 1)))
(define block-leftmost (make-block 0 4))
(define block-leftmost-moved-left (make-block -1 4))
(define block-rightmost (make-block (- WIDTH 1) 4))
(define block-rightmost-moved-right (make-block WIDTH 4))
(define block-left-block (make-block 5 (- HEIGHT 3)))
(define block-left-block-moved-right (make-block 6 (- HEIGHT 3)))
(define block-right-block (make-block 7 (- HEIGHT 3)))
(define block-right-block-moved-left (make-block 6 (- HEIGHT 3)))
(define tetris0 (make-tetris block0 landscape0))

; N -> Block
; generates a block whose x field is different from c
(check-satisfied (block-x (block-generate 5)) not=5?)
(define (block-generate c)
  (block-check-generate c (make-block (random WIDTH) 0)))

; N Block -> Block
; checks if the x filed of the candidate block is the same as c
; If so, recreate a position.
(define (block-check-generate c candidate)
  (if (= (block-x candidate) c) (block-generate c) candidate))

; N -> Boolean
; use for testing only
(check-expect (not=5? 5) #false)
(check-expect (not=5? 3) #true)
(define (not=5? n)
  (not (= n 5)))

; PositiveNumber -> Tetris
; A video game: Tetris
(define (tetris-main rate)
  (big-bang (make-tetris (block-generate WIDTH) '())
    [on-draw tetris-render]
    [on-tick tock rate]
    [on-key key-handler]
    [stop-when end?]))

; Tetris -> Image
; renders the given game state t
(check-expect (tetris-render tetris0)
              (block-render (tetris-block tetris0)
                            (landscape-render (tetris-landscape tetris0))))
(define (tetris-render t)
  (block-render (tetris-block t)
                (landscape-render (tetris-landscape t))))

; Block Image -> Image
; renders the block b on top of the scene s
(check-expect (block-render block0 SCENE)
              (place-image
               BLOCK
               (posn-x (block->posn block0))
               (posn-y (block->posn block0))
               SCENE))
(define (block-render b s)
  (place-image
   BLOCK
   (posn-x (block->posn b))
   (posn-y (block->posn b))
   s))

; Landscape -> Image
; renders the landscape l
(check-expect (landscape-render landscape0)
              (place-images
               (make-list (length landscape0) BLOCK-REST)
               (landscape->posns landscape0)
               SCENE))
(define (landscape-render l)
  (place-images
   (make-list (length l) BLOCK-REST)
   (landscape->posns l)
   SCENE))

; Block -> Posn
; converts a block to a posn
(check-expect (block->posn (make-block 0 0))
              (make-posn HALF-SIZE HALF-SIZE))
(check-expect (block->posn (make-block 1 1))
              (make-posn (+ SIZE HALF-SIZE) (+ SIZE HALF-SIZE)))
(check-expect (block->posn (make-block 2 2))
              (make-posn (+ (* 2 SIZE) HALF-SIZE)
                         (+ (* 2 SIZE) HALF-SIZE)))
(define (block->posn b)
  (make-posn (+ (* (block-x b) SIZE) HALF-SIZE)
             (+ (* (block-y b) SIZE) HALF-SIZE)))

; Landscape -> Posns
; converts a Landscape to a list of Posns
(check-expect (landscape->posns '()) '())
(check-expect
 (landscape->posns (list (make-block 0 (- HEIGHT 1))))
 (list (make-posn HALF-SIZE (+ (* (- HEIGHT 1) SIZE) HALF-SIZE))))
(define (landscape->posns l)
  (cond [(empty? l) '()]
        [else (cons (block->posn (first l))
                    (landscape->posns (rest l)))]))

; Tetris -> Tetris
; moves the block down for for every clock tick
(check-expect
 (tock (make-tetris block-dropping landscape0))
 (make-tetris block-dropping-moved-down landscape0))
(check-random
 (tock (make-tetris block-landed landscape0))
 (make-tetris (block-generate 4) (cons block-landed landscape0)))
(check-random
 (tock (make-tetris block-on-block landscape0))
 (make-tetris (block-generate 5) (cons block-on-block landscape0)))
(define (tock t)
  (next-tetris-vertical (tetris-block t)
                        (move-block (tetris-block t) DOWN)
                        (tetris-landscape t)))

; Block Block Landscape -> Tetris
; generates the next Tetris given the block b, the block moved b,
; and the landscape l
(check-expect (next-tetris-vertical block-dropping
                                    block-dropping-moved-down
                                    landscape0)
              (make-tetris block-dropping-moved-down landscape0))
(check-random (next-tetris-vertical block-landed
                                    block-landed-moved-down
                                    landscape0)
              (make-tetris (block-generate 4)
                           (cons block-landed landscape0)))
(check-random (next-tetris-vertical block-on-block
                                    block-on-block-moved-down
                                    landscape0)
              (make-tetris (block-generate 5)
                           (cons block-on-block landscape0)))
(define (next-tetris-vertical b b-moved l)
  (if (or (member? b-moved l)
          (>= (block-y b-moved) HEIGHT))
      (make-tetris (block-generate (block-x b))
                   (cons b l))
      (make-tetris b-moved l)))

; Block Direction -> Block
; moves the block down
(check-expect (move-block block-dropping DOWN)
              block-dropping-moved-down)
(check-expect (move-block block-dropping LEFT)
              block-dropping-moved-left)
(check-expect (move-block block-dropping RIGHT)
              block-dropping-moved-right)
(define (move-block b dir)
  (cond [(equal? dir DOWN)
         (make-block (block-x b)
                     (+ (block-y b) 1))]
        [(equal? dir LEFT)
         (make-block (- (block-x b) 1)
                     (block-y b))]
        [(equal? dir RIGHT)
         (make-block (+ (block-x b) 1)
                     (block-y b))]
        [else b]))

; Tetris KeyEvent -> Tetris
; controls the block
(check-expect
 (key-handler (make-tetris block-dropping landscape0) "left")
 (make-tetris block-dropping-moved-left landscape0))
(check-expect
 (key-handler (make-tetris block-dropping landscape0) "right")
 (make-tetris block-dropping-moved-right landscape0))
(check-expect
 (key-handler (make-tetris block-leftmost landscape0) "left")
 (make-tetris block-leftmost landscape0))
(check-expect
 (key-handler (make-tetris block-rightmost landscape0) "right")
 (make-tetris block-rightmost landscape0))
(check-expect
 (key-handler (make-tetris block-right-block landscape0) "left")
 (make-tetris block-right-block landscape0))
(check-expect
 (key-handler (make-tetris block-left-block landscape0) "right")
 (make-tetris block-left-block landscape0))
(define (key-handler t key)
  (cond [(string=? key "left")
         (next-tetris-horizontal (tetris-block t)
                                 (move-block (tetris-block  t) LEFT)
                                 (tetris-landscape t))]
        [(string=? key "right")
         (next-tetris-horizontal (tetris-block t)
                                 (move-block (tetris-block t) RIGHT)
                                 (tetris-landscape t))]
        [else t]))

; Block Block landscape -> Tetris
; generates the next Tetris given the block b, the moved block b-moved,
; and the landscape l
(check-expect (next-tetris-horizontal
               block-dropping
               block-dropping-moved-left
               landscape0)
              (make-tetris block-dropping-moved-left landscape0))
(check-expect (next-tetris-horizontal
               block-dropping
               block-dropping-moved-right
               landscape0)
              (make-tetris block-dropping-moved-right landscape0))
(check-expect (next-tetris-horizontal
               block-leftmost
               block-leftmost-moved-left
               landscape0)
              (make-tetris block-leftmost landscape0))
(check-expect (next-tetris-horizontal
               block-rightmost
               block-rightmost-moved-right
               landscape0)
              (make-tetris block-rightmost landscape0))
(check-expect (next-tetris-horizontal
               block-left-block
               block-left-block-moved-right
               landscape0)
              (make-tetris block-left-block landscape0))
(check-expect (next-tetris-horizontal
               block-right-block
               block-right-block-moved-left
               landscape0)
              (make-tetris block-right-block landscape0))
(define (next-tetris-horizontal b b-moved l)
  (if (or (< (block-x b-moved) 0)
          (>= (block-x b-moved) WIDTH)
          (member? b-moved l))
      (make-tetris b l)
      (make-tetris b-moved l)))

; Tetris -> Boolean
; stops when one of the columns contains enough blocks to touch
; the top of the scene
(check-expect (end? (make-tetris block0 landscape0)) #false)
(check-expect (end? (make-tetris block0 landscape-end)) #true)
(define (end? t)
  (check-end? (tetris-landscape t)))

; Landscape -> Boolean
; determines if the game ends given the landscape l
(check-expect (check-end? landscape0) #false)
(check-expect (check-end? landscape-end) #true)
(define (check-end? l)
  (cond [(empty? l) #false]
        [else (or (= (block-y (first l)) 0)
                  (check-end? (rest l)))]))

; Application
; (tetris-main 0.3)
