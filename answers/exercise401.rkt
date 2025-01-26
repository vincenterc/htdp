#lang htdp/isl+

; An S-expr (S-expression) is one of:
; – Atom
; – [List-of S-expr]
;
; An Atom is one of:
; – Number
; – String
; – Symbol

; S-expr S-expr -> Boolean
; determines whether the two S-exprs are equal
(check-expect (sexp=? 1 1) #true)
(check-expect (sexp=? 1 'a) #false)
(check-expect (sexp=? 1 '()) #false)
(check-expect (sexp=? 1 '(1)) #false)
(check-expect (sexp=? '() 1) #false)
(check-expect (sexp=? '() '()) #true)
(check-expect (sexp=? '() '(1)) #false)
(check-expect (sexp=? '(1) 1) #false)
(check-expect (sexp=? '(1) '()) #false)
(check-expect (sexp=? '(1 a "hello") '(1 a "hello")) #true)
(check-expect (sexp=? '(1 b "hello") '(1 a "hello")) #false)
(check-expect (sexp=? '(1 (a) "hello") '(1 (a) "hello")) #true)
(check-expect (sexp=? '(1 (b) "hello") '(1 (a) "hello")) #false)
(define (sexp=? s1 s2)
  (cond [(and (atom? s1) (atom? s2)) (eq? s1 s2)]
        [(and (atom? s1) (not (atom? s2))) #false]
        [(and (empty? s1) (empty? s2)) #true]
        [(and (empty? s1) (not (empty? s2))) #false]
        [(and (cons? s1) (not (cons? s2))) #false]
        [(and (cons? s1) (cons? s2))
         (and (sexp=? (first s1) (first s2))
              (sexp=? (rest s1) (rest s2)))]))

; Any -> Boolean
; determines whether the given value a is an Atom
(define (atom? a)
  (or (number? a)
      (string? a)
      (symbol? a)))
