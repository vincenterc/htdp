#lang htdp/isl

; A Word is one of:
; – '() or
; – (cons 1String Word)
; interpretation a Word is a list of 1Strings (letters)

; Word -> [List-of Word]
; creates all rearrangements of the letters in w
(check-expect (arrangements '())
              (list '()))
(check-expect (arrangements (list "a"))
              (list (list "a")))
(check-expect (arrangements (list "d" "e"))
              (list (list "d" "e") (list "e" "d")))
(define (arrangements w)
  (local
    (; 1String [List-of Word] -> [List-of Word]
     ; produces a list of words with s inserted
     ; at the beginning, between all letters,
     ; and at the end of all words of the given list low
     (define (insert-everywhere/in-all-words s low)
       (cond
         [(empty? low) '()]
         [else
          (append (insert-everywhere/word s (first low))
                  (insert-everywhere/in-all-words s (rest low)))]))

     ; 1String Word -> [List-of Word]
     ; produces a list of words with s inserted
     ; at the beginning, between all letters,
     ; and at the end of the given word w
     (define (insert-everywhere/word s w)
       (cond [(empty? w) (list (cons s '()))]
             [else (cons (cons s w)
                         (insert-at-beginning/in-all-word
                          (first w)
                          (insert-everywhere/word s (rest w))))]))

     ; 1String [List-of Word] -> [List-of Word]
     ; produces a list of words with s inserted
     ; at the beginning of all words of the given list low
     (define (insert-at-beginning/in-all-word s low)
       (cond [(empty? low) '()]
             [else
              (cons (cons s (first low))
                    (insert-at-beginning/in-all-word s (rest low)))])))
    (cond
      [(empty? w) (list '())]
      [else (insert-everywhere/in-all-words
             (first w)
             (arrangements (rest w)))])))
