#lang htdp/bsl+

; An Lo1S is one of:
; – '()
; – (cons 1String Lo1S)

; An Llo1S is one of:
; – '()
; – (cons Lo1s Llo1s)

(define list-a (list "a"))
(define list-b (list "b"))
(define list-c (list "c"))
(define list-a-b (list "a" "b"))
(define list-b-c (list "b" "c"))
(define list-a-b-c (list "a" "b" "c"))

; Lo1s -> Llo1s
; produces the list of all prefixes given l
(check-expect (prefixes '()) '())
(check-expect (prefixes list-a)
              (list list-a))
(check-expect (prefixes list-a-b)
              (list list-a-b list-a))
(check-expect (prefixes list-a-b-c)
              (list list-a-b-c list-a-b list-a))
(define (prefixes l)
  (cond [(empty? l) '()]
        [else (cons l (prefixes (remove-last l)))]))

; Lo1s -> Lo1s
; removes the last 1String in l
(check-expect (remove-last list-a)
              '())
(check-expect (remove-last list-a-b)
              list-a)
(check-expect (remove-last list-a-b-c)
              list-a-b)
(define (remove-last l)
  (cond [(empty? (rest l)) '()]
        [else (cons (first l) (remove-last (rest l)))]))

; Lo1s -> Llo1s
; produces the list of all suffixes given l
(check-expect (suffixes '()) '())
(check-expect (suffixes list-a)
              (list list-a))
(check-expect (suffixes list-a-b)
              (list list-a-b list-b))
(check-expect (suffixes list-a-b-c)
              (list list-a-b-c list-b-c list-c))
(define (suffixes l)
  (cond [(empty? l) '()]
        [else (cons l (suffixes (rest l)))]))
