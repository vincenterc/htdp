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

; S-expr Symbol Symbol -> S-expr
; produces a variation of the S-expr sexp with all occurrences of
; the symbol old-sy replaced by new-sy
(check-expect (substitute 'world 'world 'hello)
              'hello)
(check-expect (substitute '(world hello) 'world 'hello)
              '(hello hello))
(check-expect (substitute '(((world) hello) hello) 'world 'hello)
              '(((hello) hello) hello))
(check-expect (substitute '(((world) hello) hello) 'hello 'world)
              '(((world) world) world))
(define (substitute sexp old-sy new-sy)
  (local (; SL -> SL
          ; substitutes all occurrences of the symbol old-sy
          ; with new-sy in sl
          (define (substitute-sl sl)
            (cond
              [(empty? sl) '()]
              [else
               (cons (substitute (first sl) old-sy new-sy)
                     (substitute-sl (rest sl)))]))

          ; Atom -> Atom
          ; substitutes the symbol old-sy with new-sy
          (define (substitute-atom at)
            (cond
              [(number? at) at]
              [(string? at) at]
              [(symbol? at) (if (symbol=? at old-sy) new-sy at)])))
    (cond
      [(atom? sexp) (substitute-atom sexp)]
      [else (substitute-sl sexp)])))

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
