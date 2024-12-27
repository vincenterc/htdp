#lang htdp/bsl+

(require 2htdp/image)
(require 2htdp/universe)

(define RADIUS 5)
(define WORM (circle RADIUS "solid" "red"))
(define SPEED (* RADIUS 2))

(define FOOD (circle RADIUS "solid" "green"))

(define GRID# 17)

(define WIDTH (* RADIUS 2 GRID#))
(define HEIGHT (* RADIUS 2 GRID#))
(define BACKGROUND (empty-scene WIDTH HEIGHT))

(define FONT-SIZE 14)
(define FONT-COLOR "black")

; A List-of-anys is one of
; - '()
; (cons Any List-of-anys)

(define LEFT "left")
(define RIGHT "right")
(define UP "up")
(define DOWN "down")

; A Direction is one of:
; - LEFT
; - RIGHT
; - Up
; - DOWN

(define-struct worm [segs dir])
; A Worm is a structure:
;   (make-worm Lop Direction)
; A Lop is one of:
; - '()
; - (cons Posn Lop)
; interpretation (make-worm s d)
; describes a worm moving in the direction d
; with segments at positions s

(define INITIAL-WORM
  (make-worm
   (list (make-posn (/ WIDTH 2) (/ HEIGHT 2)))
   DOWN))
(define WORM-1
  (make-worm
   (list (make-posn 25 25))
   DOWN))

(define-struct gs [worm food])
; A GS is a structure:
;   (make-gs Worm Posn)
; interpretation (make-gs w f) describes a worm with its state w
; and a piece of food at position f

(define GS-1
  (make-gs WORM-1 (make-posn 55 55)))

; Lop -> Posn
; creates a piece of food at a random position that is not on the
; list of positions lop
(check-satisfied (food-create (list (make-posn 5 5))) not=-5-5?)
(define (food-create lop)
  (food-check-create
   lop (make-posn (random-position GRID#) (random-position GRID#))))

; Natural -> Natural
; generates a random position using a maximum grid number max-grid#
(define (random-position max-grid#)
  (+ RADIUS (* RADIUS 2 (random max-grid#))))

; Lop Posn -> Posn
; generative recursion
; checks if the candidate position is on the list of positions lop
; If so, recreate a position.
(define (food-check-create lop candidate)
  (if (member? candidate lop) (food-create lop) candidate))

; Posn -> Boolean
; use for testing only
(define (not=-5-5? p)
  (not (and (= (posn-x p) 5) (= (posn-y p) 5))))

; PositiveNumber Boolean -> GS
; A computer game: Worm
(define (worm-main rate state?)
  (big-bang (make-gs INITIAL-WORM
                     (food-create (worm-segs INITIAL-WORM)))
    [state state?]
    [on-draw render]
    [on-tick tock rate]
    [on-key key-handler]
    [stop-when end? render-final]))

; Worm -> GS
; generates a initial game state given a worm w
(define (initial-state w)
  (make-gs w (food-create (worm-segs w))))

; GS -> Image
; renders the given game state gs on top of BACKGROUND
(define (render gs)
  (place-image
   FOOD
   (posn-x (gs-food gs))
   (posn-y (gs-food gs))
   (render-worm (gs-worm gs))))

; Worm -> Image
; places the worm into the BACKGROUND according to w
(define (render-worm w)
  (place-images
   (make-list (length (worm-segs w)) WORM)
   (worm-segs w)
   BACKGROUND))

; GS -> GS
; moves the worm and checks if it has eaten a piece of food.
; If so, the worm extends its tail and generates a new piece of food.
(check-random (tock (make-gs (make-worm (list (make-posn 25 25)) RIGHT) (make-posn 35 25)))
              (make-gs (make-worm (list (make-posn 35 25) (make-posn 25 25)) RIGHT)
                       (food-create (list (make-posn 35 25) (make-posn 25 25)))))
(define (tock gs)
  (next-gs (gs-worm gs) (move-worm (gs-worm gs)) (gs-food gs)))

; Worm Posn -> GS
; generates the next game state given the worm w, the worm moved
; and the food f
(define (next-gs w w-moved f)
  (if (posn=? (first (worm-segs w-moved)) f)
      (make-gs (extend-worm w f)
               (food-create (worm-segs w-moved)))
      (make-gs w-moved f)))

; Posn Posn -> Boolean
(check-expect (posn=? (make-posn 10 10) (make-posn 10 10)) #true)
(check-expect (posn=? (make-posn 10 10) (make-posn 10 15)) #false)
(check-expect (posn=? (make-posn 10 10) (make-posn 15 10)) #false)
(check-expect (posn=? (make-posn 10 10) (make-posn 15 15)) #false)
(define (posn=? p1 p2)
  (and (= (posn-x p1) (posn-x p2))
       (= (posn-y p1) (posn-y p2))))

; Worm Posn -> Worm
; extends the worm given the worm w and the food f
(check-expect (extend-worm
               (make-worm (list (make-posn 20 20))
                          RIGHT)
               (make-posn 30 20))
              (make-worm (list (make-posn 30 20)
                               (make-posn 20 20))
                         RIGHT))
(define (extend-worm w f)
  (make-worm (cons f (worm-segs w))
             (worm-dir w)))

; Worm -> Worm
; moves the worm according to w
(check-expect (move-worm (make-worm (list (make-posn 20 20)
                                          (make-posn 30 20))
                                    LEFT))
              (make-worm (list (make-posn 10 20)
                               (make-posn 20 20))
                         LEFT))
(check-expect (move-worm (make-worm (list (make-posn 20 20)
                                          (make-posn 10 20))
                                    RIGHT))
              (make-worm (list (make-posn 30 20)
                               (make-posn 20 20))
                         RIGHT))
(check-expect (move-worm (make-worm (list (make-posn 20 20)
                                          (make-posn 20 30))
                                    UP))
              (make-worm (list (make-posn 20 10)
                               (make-posn 20 20))
                         UP))
(check-expect (move-worm (make-worm (list (make-posn 20 20)
                                          (make-posn 20 10))
                                    DOWN))
              (make-worm (list (make-posn 20 30)
                               (make-posn 20 20))
                         DOWN))
(define (move-worm w)
  (make-worm (move-segs (worm-segs w)
                        (next-seg (worm-segs w) (worm-dir w)))
             (worm-dir w)))

; Worm -> Posn
; get the next position of the head of the worm by given segs and dir
(check-expect (next-seg (list (make-posn 20 20)) LEFT)
              (make-posn 10 20))
(check-expect (next-seg (list (make-posn 20 20)) RIGHT)
              (make-posn 30 20))
(check-expect (next-seg (list (make-posn 20 20)) UP)
              (make-posn 20 10))
(check-expect (next-seg (list (make-posn 20 20)) DOWN)
              (make-posn 20 30))
(define (next-seg segs dir)
  (cond [(equal? dir LEFT)
         (make-posn
          (- (posn-x (first segs)) SPEED)
          (posn-y (first segs)))]
        [(equal? dir RIGHT)
         (make-posn
          (+ (posn-x (first segs)) SPEED)
          (posn-y (first segs)))]
        [(equal? dir UP)
         (make-posn
          (posn-x (first segs))
          (- (posn-y (first segs)) SPEED))]
        [(equal? dir DOWN)
         (make-posn
          (posn-x (first segs))
          (+ (posn-y (first segs)) SPEED))]
        [else (error "The direction must be one of LEFT RIGHT UP DOWN")]))

; Lop Posn -> Lop
; moves the segments of the worm according to segs and next-seg(check-expect (move-segs (list (make-posn 10 10))
(check-expect (move-segs (list (make-posn 10 10))
                         (make-posn 20 10))
              (list (make-posn 20 10)))
(check-expect (move-segs (list (make-posn 20 10)
                               (make-posn 10 10))
                         (make-posn 30 10))
              (list (make-posn 30 10)
                    (make-posn 20 10)))
(define (move-segs segs next-seg)
  (cons next-seg (remove-last segs)))

; List-of-anys -> List-of-anys
; removes the last element in l
(check-expect (remove-last (list 1))
              '())
(check-expect (remove-last (list "1" "2"))
              (list "1"))
(check-expect (remove-last (list #false #true #false))
              (list #false #true))
(define (remove-last l)
  (cond [(empty? (rest l)) '()]
        [else (cons (first l) (remove-last (rest l)))]))

; GS KeyEvent -> GS
; controls the movement of the worm with the four cardinal arrow keys
(check-expect (key-handler GS-1 "left")
              (make-gs (make-worm (worm-segs (gs-worm GS-1)) LEFT)
                       (gs-food GS-1)))
(check-expect (key-handler GS-1 "right")
              (make-gs (make-worm (worm-segs (gs-worm GS-1)) RIGHT)
                       (gs-food GS-1)))
(check-expect (key-handler GS-1 "up")
              (make-gs (make-worm (worm-segs (gs-worm GS-1)) UP)
                       (gs-food GS-1)))
(check-expect (key-handler GS-1 "down")
              (make-gs (make-worm (worm-segs (gs-worm GS-1)) DOWN)
                       (gs-food GS-1)))
(check-expect (key-handler GS-1 "a")
              GS-1)
(define (key-handler gs key)
  (cond [(string=? key "left")
         (make-gs (make-worm (worm-segs (gs-worm gs)) LEFT)
                  (gs-food gs))]
        [(string=? key "right")
         (make-gs (make-worm (worm-segs (gs-worm gs)) RIGHT)
                  (gs-food gs))]
        [(string=? key "up")
         (make-gs (make-worm (worm-segs (gs-worm gs)) UP)
                  (gs-food gs))]
        [(string=? key "down")
         (make-gs (make-worm (worm-segs (gs-worm gs)) DOWN)
                  (gs-food gs))]
        [else gs]))

; GS -> Boolean
; stops if the worm has run into the walls or into itself
(check-expect (end? GS-1) #false)
(check-expect
 (end? (make-gs (make-worm (list (make-posn 0 20)) LEFT)
                (make-posn 50 50)))
 #true)
(check-expect
 (end? (make-gs (make-worm (list (make-posn WIDTH 20)) RIGHT)
                (make-posn 50 50)))
 #true)
(check-expect
 (end? (make-gs (make-worm (list (make-posn 30 0)) UP)
                (make-posn 50 50)))
 #true)
(check-expect
 (end? (make-gs (make-worm (list (make-posn 30 WIDTH)) DOWN)
                (make-posn 50 50)))
 #true)
(check-expect
 (end? (make-gs (make-worm (list (make-posn 30 20)
                                 (make-posn 20 20)
                                 (make-posn 10 20)
                                 (make-posn 10 10)
                                 (make-posn 20 10))
                           RIGHT)
                (make-posn 50 50)))
 #false)
(check-expect
 (end? (make-gs (make-worm (list (make-posn 20 10)
                                 (make-posn 20 20)
                                 (make-posn 10 20)
                                 (make-posn 10 10)
                                 (make-posn 20 10))
                           UP)
                (make-posn 50 50)))
 #true)
(define (end? gs)
  (or (run-into-walls? (gs-worm gs))
      (run-into-self? (gs-worm gs))))

; Worm -> Boolean
; determines whether the worm has run into the walls
(check-expect (run-into-walls? (make-worm (list (make-posn 100 100)) DOWN)) #false)
(check-expect (run-into-walls? (make-worm (list (make-posn 0 20)) LEFT)) #true)
(check-expect (run-into-walls? (make-worm (list (make-posn WIDTH 20)) RIGHT)) #true)
(check-expect (run-into-walls? (make-worm (list (make-posn 30 0)) UP)) #true)
(check-expect (run-into-walls? (make-worm (list (make-posn 30 WIDTH)) DOWN)) #true)
(define (run-into-walls? w)
  (or (< (posn-x (first (worm-segs w))) RADIUS)
      (> (posn-x (first (worm-segs w))) (- WIDTH RADIUS))
      (< (posn-y (first (worm-segs w))) RADIUS)
      (> (posn-y (first (worm-segs w))) (- HEIGHT RADIUS))))

; Worm -> Boolean
; determines whether the worm has run into itself
(check-expect (run-into-self? (make-worm (list (make-posn 30 20)
                                               (make-posn 20 20)
                                               (make-posn 10 20)
                                               (make-posn 10 10)
                                               (make-posn 20 10))
                                         RIGHT))
              #false)
(check-expect (run-into-self? (make-worm (list (make-posn 20 10)
                                               (make-posn 20 20)
                                               (make-posn 10 20)
                                               (make-posn 10 10)
                                               (make-posn 20 10))
                                         UP))
              #true)
(define (run-into-self? w)
  (member? (first (worm-segs w)) (rest (worm-segs w))))

; GS -> Image
; renders the final state of the game
(define (render-final gs)
  (overlay/align/offset
   "left" "bottom"
   (text (end-text (gs-worm gs)) FONT-SIZE FONT-COLOR)
   (- (/ WIDTH 7)) (/ HEIGHT 7)
   (render gs)))

; Worm -> String
(define (end-text w)
  (cond [(run-into-walls? w)
         (string-append "worm hit border: "
                        (number->string (length (worm-segs w))))]
        [(run-into-self? w)
         (string-append "worm hit itself: "
                        (number->string (length (worm-segs w))))]))

; Application
; (worm-main 0.3 #false)
