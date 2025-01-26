#lang htdp/isl+

; An S-expr is one of:
; – Atom
; – SL

; An SL is one of:
; – '()
; – (cons S-expr SL)

; An Atom is one of:
; – Number
; – String
; – Symbol

; S-expr -> N
; determines the depth of the S-expr sexp
(check-expect (deep 'world) 1)
(check-expect (deep '(world hello a)) 2)
(check-expect (deep '(((world) hello) hello)) 4)
(define (deep sexp)
  (local ((define deep-atom 1)

          ; SL -> N
          ; determines the depth of the SL sl
          (define (deep-sl sl)
            (+ (max-deep-sl sl) 1))

          ; SL -> N
          ; determines the max depth of the items on sl
          (define (max-deep-sl sl)
            (cond [(empty? sl) 0]
                  [else (max (deep (first sl))
                             (max-deep-sl (rest sl)))])))
    (cond [(atom? sexp) deep-atom]
          [else (deep-sl sexp)])))

; Any -> Boolean
; determines whether the given value a is an Atom
(check-expect (atom? 1) #true)
(check-expect (atom? "hello") #true)
(check-expect (atom? 'a) #true)
(check-expect (atom? (make-posn 1 1)) #false)
(define (atom? a)
  (or (number? a)
      (string? a)
      (symbol? a)))
