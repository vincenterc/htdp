#lang htdp/isl+

; An S-expr is one of:
; – Number
; – String
; – Symbol
; – SL

; An SL is a [List-of S-expr]

; S-expr Symbol -> N
; counts all occurrences of sy in sexp
(check-expect (count 'world 'hello) 0)
(check-expect (count '(world hello) 'hello) 1)
(check-expect (count '(((world) hello) hello) 'hello) 2)
(define (count sexp sy)
  (local (; [List-of S-expr] -> N
          ; counts all occurrences of sy in sl
          (define (count-sl sl)
            (foldl (lambda (se occ)
                     (+ (count se sy) occ))
                   0
                   sl)))
    (cond
      [(number? sexp) 0]
      [(string? sexp) 0]
      [(symbol? sexp) (if (symbol=? sexp sy) 1 0)]
      [else (count-sl sexp)])))
