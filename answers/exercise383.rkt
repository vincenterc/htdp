#lang htdp/isl+

(require 2htdp/universe)
(require 2htdp/image)

; An Xexpr is a list:
; – (cons Symbol Body)
; – (cons Symbol (cons [List-of Attribute] Body))
; where Body is short for [List-of Xexpr]
; An Attribute is a list of two items:
;   (cons Symbol (cons String '()))

; An FSM is a [List-of 1Transition]
; A 1Transition is a list of two items:
;   (cons FSM-State (cons FSM-State '()))
; An FSM-State is a String that specifies a color
(define fsm-traffic
  '(("red" "green") ("green" "yellow") ("yellow" "red")))

; An XMachine is a nested list of this shape:
;   (cons 'machine (cons `((initial ,FSM-State)) [List-of X1T]))
; An X1T is a nested list of this shape:
;   `(action ((state ,FSM-State) (next ,FSM-State)))
(define xm0
  '(machine ((initial "red"))
            (action ((state "red") (next "green")))
            (action ((state "green") (next "yellow")))
            (action ((state "yellow") (next "red")))))
(define xm1
  '(machine ((initial "black"))
            (action ((state "black") (next "white")))
            (action ((state "white") (next "black")))))

; XMachine -> FSM-State
; interprets the given configuration as a state machine
(define (simulate-xmachine xm)
  (simulate (xm-state0 xm) (xm->transitions xm)))

; XMachine -> FSM-State
; extracts and translates the transition table from xm0
(check-expect (xm-state0 xm0) "red")
(define (xm-state0 xm0)
  (find-attr (xexpr-attr xm0) 'initial))

; XMachine -> [List-of 1Transition]
; extracts the transition table from xm
(check-expect (xm->transitions xm0) fsm-traffic)
(define (xm->transitions xm)
  (local (; X1T -> 1Transition
          (define (xaction->action xa)
            (list (find-attr (xexpr-attr xa) 'state)
                  (find-attr (xexpr-attr xa) 'next))))
    (map xaction->action (xexpr-content xm))))

; FSM-State FSM -> FSM-State
; matches the keys pressed by a player with the given FSM
(define (simulate state0 transitions)
  (big-bang state0 ; FSM-State
    [to-draw
     (lambda (current)
       (square 100 "solid" current))]
    [on-key
     (lambda (current key-event)
       (find transitions current))]))

; [X Y] [List-of [List X Y]] X -> Y
; finds the matching Y for the given X in alist
(define (find alist x)
  (local ((define fm (assoc x alist)))
    (if (cons? fm) (second fm) (error "not found"))))

; [List-of Attribute] Symbol -> [Maybe String]
; retrieves the string which is associated with sy in loa
; #false if no such string
(define (find-attr loa sy)
  (local ((define assq-result (assq sy loa)))
    (if (false? assq-result)
        #false
        (second assq-result))))

; Xexpr -> [List-of Attribute]
; retrieves the list of attributes of xe
(define (xexpr-attr xe)
  (local ((define optional-loa+content (rest xe)))
    (cond
      [(empty? optional-loa+content) '()]
      [else
       (local ((define loa-or-x
                 (first optional-loa+content)))
         (if (list-of-attributes? loa-or-x)
             loa-or-x
             '()))])))

; Xexpr -> [List-of Xexpr]
; retrieves the list of content elements of xe
(define (xexpr-content xe)
  (local ((define optional-loa+content (rest xe)))
    (cond
      [(empty? optional-loa+content) '()]
      [else
       (if (list-of-attributes? (first optional-loa+content))
           (rest optional-loa+content)
           optional-loa+content)])))

; [List-of Attribute] or Xexpr -> Boolean
; is x a list of attributes
(define (list-of-attributes? x)
  (cond
    [(empty? x) #true]
    [else
     (local ((define possible-attribute (first x)))
       (cons? possible-attribute))]))
