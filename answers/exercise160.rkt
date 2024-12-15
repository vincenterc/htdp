#lang htdp/bsl

; A Son.L is one of:
; – empty
; – (cons Number Son.L)
;
; Son is used when it
; applies to Son.L and Son.R

; A Son.R is one of:
; – empty
; – (cons Number Son.R)

; Constraint If s is a Son.R,
; no number occurs twice in s

; Son
(define es '())

; Number Son -> Boolean
; is x in s
(define (in? x s)
  (member? x s))

; Number Son.L -> Son.L
; removes x from s
(define s1.L
  (cons 1 (cons 1 '())))
(check-expect
 (set-.L 1 s1.L) es)
(define (set-.L x s)
  (remove-all x s))

; Number Son.L -> Son.L
; add x to s
(define s2.L
  (cons 1 '()))
(check-expect
 (set+.L 1 es) s2.L)
(check-expect
 (set+.L 1 s2.L) (cons 1 s2.L))
(define (set+.L x s)
  (cons x s))

; Number Son.R -> Son.R
; removes x from s
(define s1.R
  (cons 1 '()))
(check-expect
 (set-.R 1 s1.R) es)
(define (set-.R x s)
  (remove x s))

; Number Son.R -> Son.R
; add x to s
(check-expect
 (set+.R 1 es) s1.R)
(check-expect
 (set+.R 1 s1.R) s1.R)
(define (set+.R x s)
  (if (in? x s)
      s
      (cons x s)))
