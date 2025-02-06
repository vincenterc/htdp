#lang htdp/isl+

(define-struct transition [current key next])
(define-struct fsm [initial transitions final])
; An FSM is a structure:
;   (make-fsm FSM-State [List-of 1Transition] FSM-State)
; A 1Transition is a structure:
;   (make-transition FSM-State 1String FSM-State)
; An FSM-State is String.

(define fsm-a-bc*-d
  (make-fsm
   "AA"
   (list (make-transition "AA" "a" "BC")
         (make-transition "BC" "b" "BC")
         (make-transition "BC" "c" "BC")
         (make-transition "BC" "d" "DD"))
   "DD"))

; FSM String -> Boolean
; does an-fsm recognize the given string
(check-expect (fsm-match? fsm-a-bc*-d "acbd") #true)
(check-expect (fsm-match? fsm-a-bc*-d "ad") #true)
(check-expect (fsm-match? fsm-a-bc*-d "abcbbbcd") #true)
(check-expect (fsm-match? fsm-a-bc*-d "da") #false)
(check-expect (fsm-match? fsm-a-bc*-d "aa") #false)
(check-expect (fsm-match? fsm-a-bc*-d "d") #false)
(check-expect (fsm-match? fsm-a-bc*-d "ab") #false)
(define (fsm-match? an-fsm a-string)
  (local ((define transitions (fsm-transitions an-fsm))
          (define final (fsm-final an-fsm))
          ; FSM-State String -> Boolean
          (define (fsm-match?-aux cs str)
            (cond
              [(= (string-length str) 0)
               (string=? cs final)]
              [else
               (local ((define 1s (string-ith str 0))
                       ; [List-of 1Transition]
                       (define ts-matched
                         (filter
                          (lambda (t)
                            (and (string=? (transition-current t) cs)
                                 (string=? (transition-key t) 1s)))
                          transitions))
                       ; [Maybe FSM-State]
                       (define next-state
                         (if (cons? ts-matched)
                             (transition-next (first ts-matched))
                             #false)))
                 (if (string? next-state)
                     (fsm-match?-aux next-state (substring str 1))
                     #false))])))
    (fsm-match?-aux (fsm-initial an-fsm) a-string)))
