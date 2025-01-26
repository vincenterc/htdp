#lang htdp/isl+

(define NAMES '("Louise" "Jane" "Laura" "Dana" "Mary"))

; [List-of String] -> [List-of String]
; picks a random non-identity arrangement of names
(define (gift-pick names)
  (random-pick
   (non-same names (arrangements names))))

; [List-of String] -> [List-of [List-of String]]
; returns all possible permutations of names
(check-expect (arrangements '())
              (list '()))
(check-expect (arrangements (list "a"))
              (list (list "a")))
(check-expect (arrangements (list "d" "e"))
              (list (list "d" "e") (list "e" "d")))
(define (arrangements los)
  (local
    (; String [List-of [List-of String]] -> [List-of [List-of String]]
     ; produces a list of lists of strings with s inserted at the beginning,
     ; between all strings, and at the end of all lists of strings
     ; of the given list llos
     (define (insert-everywhere/in-all-lists-strings s llos)
       (cond [(empty? llos) '()]
             [else
              (append
               (insert-everywhere/list-strings s (first llos))
               (insert-everywhere/in-all-lists-strings s (rest llos)))]))
     ; String [List-of String] -> [List-of [List-of String]]
     ; produces a list of lists of strings with s inserted
     ; at the beginning, between all strings,
     ; and at the end of the given list los0
     (define (insert-everywhere/list-strings s los0)
       (cond [(empty? los0) (list (cons s '()))]
             [else (cons
                    (cons s los0)
                    (insert-at-beginning/in-all-lists-strings
                     (first los0)
                     (insert-everywhere/list-strings s (rest los0))))]))
     ; String [List-of [List-of String]] -> [List-of [List-of String]]
     ; produces a list of lists of strings with s inserted
     ; at the beginning of all lists of strings of the given list llos
     (define (insert-at-beginning/in-all-lists-strings s llos)
       (cond [(empty? llos) '()]
             [else
              (cons
               (cons s (first llos))
               (insert-at-beginning/in-all-lists-strings s (rest llos)))])))
    (cond
      [(empty? los) (list '())]
      [else (insert-everywhere/in-all-lists-strings
             (first los)
             (arrangements (rest los)))])))

; [NEList-of X] -> X
; returns a random item from the list
(check-random
 (random-pick '(1 2 3))
 (list-ref '(1 2 3) (random 3)))
(check-random
 (random-pick '(1 2 3 4))
 (list-ref '(1 2 3 4) (random 4)))
(define (random-pick l)
  (list-ref l (random (length l))))

; [List-of String] [List-of [List-of String]]
; ->
; [List-of [List-of String]]
; produces the list of those lists in ll that do
; not agree with names at any place
(check-expect
 (non-same '("a" "b" "c") '())
 '())
(check-expect
 (non-same '("a" "b")
           '(("a" "b") ("b" "a")))
 '(("b" "a")))
(check-expect
 (non-same '("a" "b" "c")
           '(("a" "b" "c")
             ("a" "c" "b")
             ("b" "a" "c")
             ("b" "c" "a")
             ("c" "a" "b")
             ("c" "b" "a")))
 '(("b" "c" "a") ("c" "a" "b")))
(define (non-same names ll)
  (local ((define (non-same?/list names0 l)
            (cond [(empty? names0) #true]
                  [else (and (not (string=? (first names0) (first l)))
                             (non-same?/list (rest names0) (rest l)))])))
    (cond [(empty? ll) '()]
          [else
           (if (non-same?/list names (first ll))
               (cons (first ll) (non-same names (rest ll)))
               (non-same names (rest ll)))])))

; Application:
; (gift-pick NAMES)
