#lang htdp/isl+

(require 2htdp/universe)
(require 2htdp/image)

; An FSM is a [List-of 1Transition]
; A 1Transition is a list of two items:
;   (cons FSM-State (cons FSM-State '()))
; An FSM-State is a String that specifies a color

; data examples
(define fsm-traffic
  '(("red" "green") ("green" "yellow") ("yellow" "red")))

; FSM-State FSM -> FSM-State
; matches the keys pressed by a player with the given FSM
(define (simulate state0 transitions)
  (big-bang state0 ; FSM-State
    [to-draw
     (lambda (current)
       (overlay
        (text current 20 "black")
        (square 100 "solid" current)))]
    [on-key
     (lambda (current key-event)
       (find transitions current))]))

; [X Y] [List-of [List X Y]] X -> Y
; finds the matching Y for the given X in alist
(check-expect (find fsm-traffic "red") "green")
(check-error (find fsm-traffic "black") "not found")
(check-expect (find '(("world" 2) ("hello" 3) ("good" 0))
                    "hello")
              3)
(check-expect (find '((a 22) (b 8) (c 70))
                    'b)
              8)
(define (find alist x)
  (local ((define fm (assoc x alist)))
    (if (cons? fm) (second fm) (error "not found"))))

; Application
; (simulate "red" fsm-traffic)
