#lang htdp/bsl+

(require 2htdp/image)
(require 2htdp/universe)

(define RADIUS 5)
(define WORM (circle RADIUS "solid" "red"))
(define SPEED (* RADIUS 2))

(define WIDTH (* RADIUS 2 17))
(define HEIGHT (* RADIUS 2 17))
(define BACKGROUND (empty-scene WIDTH HEIGHT))

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

; PositiveNumber -> Worm
; A computer game: Worm
(define (worm-main rate)
  (big-bang INITIAL-WORM
    [on-draw render]
    [on-tick tock rate]
    [on-key key-handler]))

; Worm -> Image
; places the worm into the BACKGROUND according to w
(define (render w)
  (render-segs (worm-segs w)))

; Lop -> Image
; renders the segments of the worm according to segs
(define (render-segs segs)
  (cond [(empty? segs) BACKGROUND]
        [else (place-image
               WORM
               (posn-x (first segs))
               (posn-y (first segs))
               (render-segs (rest segs)))]))

; Worm -> Worm
; moves the worm according to w
(check-expect (tock (make-worm (list (make-posn 20 20)
                                     (make-posn 30 20))
                               LEFT))
              (make-worm (list (make-posn 10 20)
                               (make-posn 20 20))
                         LEFT))
(check-expect (tock (make-worm (list (make-posn 20 20)
                                     (make-posn 10 20))
                               RIGHT))
              (make-worm (list (make-posn 30 20)
                               (make-posn 20 20))
                         RIGHT))
(check-expect (tock (make-worm (list (make-posn 20 20)
                                     (make-posn 20 30))
                               UP))
              (make-worm (list (make-posn 20 10)
                               (make-posn 20 20))
                         UP))
(check-expect (tock (make-worm (list (make-posn 20 20)
                                     (make-posn 20 10))
                               DOWN))
              (make-worm (list (make-posn 20 30)
                               (make-posn 20 20))
                         DOWN))
(define (tock w)
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

; Worm KeyEvent -> Worm
; controls the movement of the worm with the four cardinal arrow keys
(check-expect (key-handler WORM-1 "left")
              (make-worm (worm-segs WORM-1) LEFT))
(check-expect (key-handler WORM-1 "right")
              (make-worm (worm-segs WORM-1) RIGHT))
(check-expect (key-handler WORM-1 "up")
              (make-worm (worm-segs WORM-1) UP))
(check-expect (key-handler WORM-1 "down")
              (make-worm (worm-segs WORM-1) DOWN))
(check-expect (key-handler WORM-1 "a")
              WORM-1)
(define (key-handler w key)
  (cond [(string=? key "left")
         (make-worm (worm-segs w) LEFT)]
        [(string=? key "right")
         (make-worm (worm-segs w) RIGHT)]
        [(string=? key "up")
         (make-worm (worm-segs w) UP)]
        [(string=? key "down")
         (make-worm (worm-segs w) DOWN)]
        [else w]))

; Application
; (worm-main 0.5)
