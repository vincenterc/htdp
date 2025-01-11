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

; S-expr Symbol Atom -> S-expr
; replaces all occurrences of old in sexp with new
(check-expect (substitute '(((world) bye) bye) 'bye '42)
              '(((world) 42) 42))
(check-expect (substitute '(world hello) 'world 'hello)
              '(hello hello))
(check-expect (substitute '(((world) hello) hello) 'world 'hello)
              '(((hello) hello) hello))
(define (substitute sexp old new)
  (cond
    [(atom? sexp) (if (equal? sexp old) new sexp)]
    [else
     (map (lambda (s) (substitute s old new)) sexp)]))

; Atom -> Boolean
; determines whether the given value a is an Atom
(check-expect (atom? 1) #true)
(check-expect (atom? "hello") #true)
(check-expect (atom? 'a) #true)
(check-expect (atom? (make-posn 1 1)) #false)
(define (atom? a)
  (or (number? a)
      (string? a)
      (symbol? a)))
