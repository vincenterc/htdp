#lang htdp/bsl+

; An Lo1S is one of:
; – '()
; – (cons 1String Lo1S)

; An Llo1S is one of:
; – '()
; – (cons Lo1s Llo1s)

; Lo1s -> Llo1s
; produces the list of all prefixes given l
(check-expect (prefixes '()) '())
(check-expect (prefixes (list 1))
              (list (list 1)))
(check-expect (prefixes (list 1 2))
              (list (list 1 2) (list 1)))
(check-expect (prefixes (list 1 2 3))
              (list (list 1 2 3) (list 1 2) (list 1)))
(define (prefixes l)
  (cond [(empty? l) '()]
        [else (cons l (prefixes (reverse (rest (reverse l)))))]))

;; Lo1s -> Llo1s
; produces the list of all suffixes given l
(check-expect (suffixes '()) '())
(check-expect (suffixes (list 1))
              (list (list 1)))
(check-expect (suffixes (list 1 2))
              (list (list 1 2) (list 2)))
(check-expect (suffixes (list 1 2 3))
              (list (list 1 2 3) (list 2 3) (list 3)))
(define (suffixes l)
  (cond [(empty? l) '()]
        [else (cons l (suffixes (rest l)))]))
