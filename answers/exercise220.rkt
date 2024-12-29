#lang htdp/bsl+

(require 2htdp/image)

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

; A Posns is one of:
; - '()
; - (cons Posn Lop)

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

(define landscape0 (list (make-block 5 (- HEIGHT 1))
                         (make-block 6 (- HEIGHT 1))))
(define block0 (make-block 2 0))
(define block-dropping (make-block 2 5))
(define block-landed (make-block 2 (- HEIGHT 1)))
(define block-on-block (make-block 5 (- HEIGHT 2)))
(define tetris0 (make-tetris block0 landscape0))

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
