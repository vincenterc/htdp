#lang htdp/isl+

; [NEList-of 1String] -> [NEList-of 1String]
; creates a palindrome from s0
(check-expect
 (mirror (explode "abc")) (explode "abcba"))
(define (mirror s0)
  (append (all-but-last s0)
          (list (last s0))
          (reverse (all-but-last s0))))

; [NEList-of 1String] -> [NEList-of 1String]
; removes the last 1String from s
(check-expect (all-but-last '(1)) '())
(check-expect (all-but-last '(1 2)) '(1))
(check-expect (all-but-last '(1 2 3)) '(1 2))
(define (all-but-last s)
  (cond [(empty? (rest s)) '()]
        [else (cons (first s)
                    (all-but-last (rest s)))]))

; [NEList-of 1String] -> 1String
; extracts the last 1String from s
(check-expect (last '(1)) 1)
(check-expect (last '(1 2)) 2)
(check-expect (last '(1 2 3)) 3)
(define (last s)
  (cond [(empty? (rest s)) (first s)]
        [else (last (rest s))]))

; [NEList-of 1String] -> [NEList-of 1String]
; creates a palindrome from s0
(check-expect
 (mirror.v2 (explode "abc")) (explode "abcba"))
(define (mirror.v2 s0)
  (local (; [NEList-of 1String] -> [NEList-of 1String]
          ; creates a palindrome from s0
          ; accumulator a is a list of the 1Strings that
          ; s lacks from s0 in reverse order
          (define (mirror/a s a)
            (cond
              [(empty? (rest s))
               (cons (first s) a)]
              [else (cons (first s)
                          (mirror/a (rest s)
                                    (cons (first s) a)))])))
    (mirror/a s0 '())))
