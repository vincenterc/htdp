#lang htdp/isl+

; An S-expr is one of:
; – Number
; – String
; – Symbol
; – [List-of S-expr]

; S-expr Symbol -> N
; counts all occurrences of sy in sexp
(check-expect (count 'world 'hello) 0)
(check-expect (count '(world hello) 'hello) 1)
(check-expect (count '(((world) hello) hello) 'hello) 2)
(define (count sexp sy)
  (cond
    [(number? sexp) 0]
    [(string? sexp) 0]
    [(symbol? sexp) (if (symbol=? sexp sy) 1 0)]
    [else (foldl (lambda (se occ)
                   (+ (count se sy) occ))
                 0
                 sexp)]))
