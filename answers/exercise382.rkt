#lang htdp/isl+

; An XMachine is a nested list of this shape:
;   (cons 'machine (cons `((initial ,FSM-State)) [List-of X1T]))
; An X1T is a nested list of this shape:
;   `(action ((state ,FSM-State) (next ,FSM-State)))

; <machine initial="black">
;   <action state="black"  next="white" />
;   <action state="white"  next="black" />
; </machine>

(define xm1
  '(machine ((initial "black"))
            (action ((state "black") (next "white")))
            (action ((state "white") (next "black")))))
