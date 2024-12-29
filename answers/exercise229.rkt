#lang htdp/bsl+

(require 2htdp/image)
(require 2htdp/universe)

; An FSM.v2 is one of:
;   – '()
;   – (cons Transition.v2 FSM.v2)
(define-struct ktransition [current key next])
; A Transition.v2 is a structure:
;   (make-ktransition FSM-State.v2 KeyEvent FSM-State.v2)
; FSM-State.v2 is on of:
; - AA
; - BB
; - DD
; interpretation An FSM.v2 represents the transitions that a
; finite state machine can take from one state to another
; in reaction to keystrokes

(define AA "start, expect an 'a'")
(define BB "expect 'b', 'c', or 'd'")
(define DD "finished")

(define regex
  (list (make-ktransition AA "a" BB)
        (make-ktransition BB "b" BB)
        (make-ktransition BB "c" BB)
        (make-ktransition BB "d" DD)))

(define AA-COLOR "white")
(define BB-COLOR "yellow")
(define DD-COLOR "green")

(define-struct fs2 [fsm2 current])
; A SimulationState.v3 is a structure:
;   (make-fs2 FSM.v2 FSM-State.v2)

; FSM.v2 FSM-State.v2 -> SimulationState.v3
; match the keys pressed with the given FSM.v2
(define (simulate.v3 an-fsm2 s0)
  (big-bang (make-fs2 an-fsm2 s0)
    [to-draw state-as-colored-square.v2]
    [on-key find-next-state.v2]))

; SimulationState.v3 -> Image
; renders current world state as a colored square
(check-expect (state-as-colored-square.v2
               (make-fs2 regex AA))
              (square 100 "solid" AA-COLOR))
(define (state-as-colored-square.v2 fs2)
  (square 100 "solid" (state->color (fs2-current fs2))))

; FSM-State.v2 -> String
; generates a color string, given s.
(define (state->color s)
  (cond [(string=? s AA) AA-COLOR]
        [(string=? s BB) BB-COLOR]
        [(string=? s DD) DD-COLOR]
        [else "state not found"]))

; SimulationState.v3 KeyEvent -> SimulationState.v3
; finds the next state from fs2 and ke
(check-expect
 (find-next-state.v2 (make-fs2 regex AA) "a")
 (make-fs2 regex BB))
(check-expect
 (find-next-state.v2 (make-fs2 regex BB) "b")
 (make-fs2 regex BB))
(check-expect
 (find-next-state.v2 (make-fs2 regex BB) "c")
 (make-fs2 regex BB))
(check-expect
 (find-next-state.v2 (make-fs2 regex BB) "d")
 (make-fs2 regex DD))
(define (find-next-state.v2 fs2 ke)
  (make-fs2
   (fs2-fsm2 fs2)
   (find.v2 (fs2-fsm2 fs2) (fs2-current fs2) ke)))

; FSM.v2 FSM-State.v2 KeyEvent -> FSM-State.v2
; finds the state representing current in transitions
; and retrieves the next field
(check-expect (find.v2 regex AA "a") BB)
(check-expect (find.v2 regex BB "b") BB)
(check-expect (find.v2 regex BB "c") BB)
(check-expect (find.v2 regex BB "d") DD)
(check-error (find.v2 regex AA "n")
             "start, expect an 'a', but found 'n'")
(define (find.v2 transitions current key)
  (cond [(empty? transitions)
         (error (string-append current ", but found '" key "'"))]
        [else
         (if (and (state=?.v2 (ktransition-current (first transitions))
                              current)
                  (string=? (ktransition-key (first transitions))
                            key))
             (ktransition-next (first transitions))
             (find.v2 (rest transitions) current key))]))

; FSM-State.v2 FSM-State.v2 -> Boolean
; determines whether s1 and s2 are the same
(check-expect (state=?.v2 AA BB) #false)
(check-expect (state=?.v2 AA AA) #true)
(define (state=?.v2 s1 s2)
  (string=? s1 s2))

; Application
; (simulate.v3 regex AA)
