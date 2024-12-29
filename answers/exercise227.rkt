#lang htdp/bsl+

; An FSM is one of:
;   – '()
;   – (cons Transition FSM)
(define-struct transition [current next])
; A Transition is a structure:
;   (make-transition FSM-State FSM-State)
; FSM-State is a Color.
; interpretation An FSM represents the transitions that a
; finite state machine can take from one state to another
; in reaction to keystrokes

(define BW-Machine
  (list (make-transition "black" "white")
        (make-transition "white" "black")))
