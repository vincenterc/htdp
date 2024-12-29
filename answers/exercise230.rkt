#lang htdp/bsl+

(require 2htdp/image)
(require 2htdp/universe)

(define-struct fsm [initial transitions final])
(define-struct transition [current key next])
; An FSM.v3 is a structure:
;   (make-fsm FSM-State.v2 LOT FSM-State.v2)
; A LOT is one of:
; - '()
; – (cons Transition.v3 LOT)
; A Transition.v3 is a structure:
;   (make-transition FSM-State.v2 KeyEvent FSM-State.v2)
; FSM-State.v2 is on of:
; - AA
; - BB
; - DD
; interpretation An FSM.v3 represents the transitions that a
; finite state machine can take from one state to another
; in reaction to keystrokes

(define AA "start, expect an 'a'")
(define BB "expect 'b', 'c', or 'd'")
(define DD "finished")

(define transitions
  (list (make-transition AA "a" BB)
        (make-transition BB "b" BB)
        (make-transition BB "c" BB)
        (make-transition BB "d" DD)))

(define regex0
  (make-fsm AA transitions DD))

(define AA-COLOR "white")
(define BB-COLOR "yellow")
(define DD-COLOR "green")

; FSM.v3 -> FSM.v3
; match the keys pressed with the given FSM.v3
(define (fsm-simulate fsm3)
  (big-bang fsm3
    [to-draw state-as-colored-square.v3]
    [on-key find-next-state.v3]
    [stop-when end? state-as-colored-square.v3]))

; FSM.v3 -> Image
; renders current world state as a colored square
(check-expect (state-as-colored-square.v3 regex0)
              (square 100 "solid" AA-COLOR))
(define (state-as-colored-square.v3 fsm3)
  (square 100 "solid" (state->color (fsm-initial fsm3))))

; FSM-State.v2 -> String
; generates a color string, given s.
(define (state->color s)
  (cond [(string=? s AA) AA-COLOR]
        [(string=? s BB) BB-COLOR]
        [(string=? s DD) DD-COLOR]
        [else "state not found"]))

; FSM.v3 KeyEvent -> FSM.v3
; finds the next state from fsm3 and ke
(check-expect
 (find-next-state.v3 (make-fsm AA transitions DD) "a")
 (make-fsm BB transitions DD))
(check-expect
 (find-next-state.v3 (make-fsm BB transitions DD) "b")
 (make-fsm BB transitions DD))
(check-expect
 (find-next-state.v3 (make-fsm BB transitions DD) "c")
 (make-fsm BB transitions DD))
(check-expect
 (find-next-state.v3 (make-fsm BB transitions DD) "d")
 (make-fsm DD transitions DD))
(define (find-next-state.v3 fsm3 ke)
  (make-fsm
   (find.v3 (fsm-initial fsm3) (fsm-transitions fsm3) ke)
   (fsm-transitions fsm3)
   (fsm-final fsm3)))

; FSM-State.v2 LOT KeyEvent -> FSM-State.v2
; finds the state representing current in transitions
; and retrieves the next field
(check-expect (find.v3 AA transitions "a") BB)
(check-expect (find.v3 BB transitions "b") BB)
(check-expect (find.v3 BB transitions "c") BB)
(check-expect (find.v3 BB transitions "d") DD)
(check-error (find.v3 AA transitions "n")
             "start, expect an 'a', but found 'n'")
(define (find.v3 current transitions key)
  (cond [(empty? transitions)
         (error (string-append current ", but found '" key "'"))]
        [else
         (if (and (state=?.v2 (transition-current (first transitions))
                              current)
                  (string=? (transition-key (first transitions))
                            key))
             (transition-next (first transitions))
             (find.v3 current (rest transitions) key))]))

; FSM-State.v2 FSM-State.v2 -> Boolean
; determines whether s1 and s2 are the same
(check-expect (state=?.v2 AA BB) #false)
(check-expect (state=?.v2 AA AA) #true)
(define (state=?.v2 s1 s2)
  (string=? s1 s2))

; FSM.v3 -> Boolean
; stops if the sequence of keystrokes forces the FSM.v3 to reach a final state
(check-expect (end? (make-fsm AA transitions DD)) #false)
(check-expect (end? (make-fsm DD transitions DD)) #true)
(define (end? fsm3)
  (state=?.v2 (fsm-initial fsm3) (fsm-final fsm3)))

; Application
; (fsm-simulate regex0)
