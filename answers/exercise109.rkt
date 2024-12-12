#lang htdp/bsl

(require 2htdp/image)
(require 2htdp/universe)

; ExpectsToSee.v2 is one of:
; – AA
; – BB
; – DD
; – ER

(define AA "start, expect an 'a'")
(define BB "expect 'b', 'c', or 'd'")
(define DD "finished")
(define ER "error, illegal key")

(define WIDTH 100)
(define HEIGHT 100)

(define AA-COLOR "white")
(define BB-COLOR "yellow")
(define DD-COLOR "green")
(define ER-COLOR "red")

; ExpectsToSee.v2 -> ExpectsToSee.v2
; recognizes a pattern in a sequence of KeyEvents.
(define (sequence-recognition s)
  (big-bang s
    [on-draw render]
    [on-key recognize]
    [stop-when stop? render]))

; ExpectsToSee.v2 -> Image
; Display a white rectangle given AA,
; yellow given BB, green given DD and red given ER.
(check-expect (render AA)
              (rectangle WIDTH HEIGHT "solid" "white"))
(check-expect (render BB)
              (rectangle WIDTH HEIGHT "solid" "yellow"))
(check-expect (render DD)
              (rectangle WIDTH HEIGHT "solid" "green"))
(check-expect (render ER)
              (rectangle WIDTH HEIGHT "solid" "red"))
(define (render s)
  (rectangle WIDTH HEIGHT "solid" (state->color s)))

; ExpectsToSee.v2 -> String
; generates a color string, given s.
(define (state->color s)
  (cond [(string=? s AA) AA-COLOR]
        [(string=? s BB) BB-COLOR]
        [(string=? s DD) DD-COLOR]
        [(string=? s ER) ER-COLOR]))

; ExpectsToSee.v2 KeyEvent -> ExpectsToSee.v2
; changes the state according one of the following:
; - AA + "a" -> BB
;      + not "a" -> ER
; - BB + "b" or "c" -> BB
;      + "d" -> DD
;      + else -> ER
(check-expect (recognize AA "a") BB)
(check-expect (recognize AA "b") ER)
(check-expect (recognize BB "b") BB)
(check-expect (recognize BB "c") BB)
(check-expect (recognize BB "d") DD)
(check-expect (recognize BB "a") ER)
(define (recognize s key)
  (cond [(and (string=? s AA) (string=? key "a")) BB]
        [(and (string=? s BB) (string=? key "d")) DD]
        [(and (string=? s BB) (or (string=? key "b")
                                  (string=? key "c"))) BB]
        [else ER]))

; ExpectsToSee.v2 -> Boolean
; returns #true given DD or ER, otherwise returns #false.
(check-expect (stop? DD) #true)
(check-expect (stop? ER) #true)
(check-expect (stop? AA) #false)
(check-expect (stop? BB) #false)
(define (stop? s)
  (cond [(or (string=? s DD) (string=? s ER)) #true]
        [else #false]))

; Application
; (sequence-recognition AA)
