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

; FSM-State FSM-State -> Boolean
; determines whether s1 and s2 are the same
(check-expect (state=? "green" "red") #false)
(check-expect (state=? "green" "green") #true)
(define (state=? s1 s2)
  (string=? s1 s2))
