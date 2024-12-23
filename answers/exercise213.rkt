#lang htdp/bsl+

; A Word is one of:
; – '() or
; – (cons 1String Word)
; interpretation a Word is a list of 1Strings (letters)

; A List-of-words is one of:
; - '()
; - (cons Word List-of-words)

; 1String List-of-words -> List-of-words
; produces a list of words with s inserted at the beginning,
; between all letters, and at the end of all words of the given list low
(check-expect (insert-everywhere/in-all-words "e" (list '()))
              (list (list "e")))
(check-expect (insert-everywhere/in-all-words "d" (list (list "e")))
              (list (list "d" "e") (list "e" "d")))
(check-expect
 (insert-everywhere/in-all-words "d" (list (list "e" "f") (list "f" "e")))
 (list (list "d" "e" "f") (list "e" "d" "f") (list "e" "f" "d")
       (list "d" "f" "e") (list "f" "d" "e") (list "f" "e" "d")))
(define (insert-everywhere/in-all-words s low)
  (cond [(empty? low) '()]
        [else (append (prepend-insert-everywhere/word '() s (first low))
                      (insert-everywhere/in-all-words s (rest low)))]))

; Word 1String Word -> List-of-words
; produces a list of words by inserting s at the beginning,
; between all letters, and at the end of post and prepending pre to
; those words
(check-expect (prepend-insert-everywhere/word '() "e" '())
              (list (list "e")))
(check-expect (prepend-insert-everywhere/word '() "d" (list "e"))
              (list (list "d" "e") (list "e" "d")))
(check-expect (prepend-insert-everywhere/word (list "e") "d" '())
              (list (list "e" "d")))
(check-expect
 (prepend-insert-everywhere/word '() "d" (list "e" "f"))
 (list (list "d" "e" "f") (list "e" "d" "f") (list "e" "f" "d")))
(check-expect
 (prepend-insert-everywhere/word (list "e") "d" (list "f"))
 (list (list "e" "d" "f") (list "e" "f" "d")))
(check-expect
 (prepend-insert-everywhere/word (list "e" "f") "d" '())
 (list  (list "e" "f" "d")))
(define (prepend-insert-everywhere/word pre s word)
  (cond [(empty? word) (list (append pre (cons s '())))]
        [else (cons (append pre (cons s word))
                    (prepend-insert-everywhere/word
                     (append pre (list (first word)))
                     s
                     (rest word)))]))

; 1String List-of-words -> List-of-words
; produces a list of words with s inserted at the beginning,
; between all letters, and at the end of all words of the given list low
(check-expect (insert-everywhere/in-all-words.v2 "e" (list '()))
              (list (list "e")))
(check-expect (insert-everywhere/in-all-words.v2 "d" (list (list "e")))
              (list (list "d" "e") (list "e" "d")))
(check-expect
 (insert-everywhere/in-all-words.v2 "d" (list (list "e") (list "f")))
 (list (list "d" "e") (list "e" "d")
       (list "d" "f") (list "f" "d")))
(check-expect
 (insert-everywhere/in-all-words.v2 "d" (list (list "e" "f")
                                              (list "f" "e")))
 (list (list "d" "e" "f") (list "e" "d" "f") (list "e" "f" "d")
       (list "d" "f" "e") (list "f" "d" "e") (list "f" "e" "d")))
(define (insert-everywhere/in-all-words.v2 s low)
  (cond [(empty? low) '()]
        [else
         (append (insert-everywhere/word s (first low))
                 (insert-everywhere/in-all-words.v2 s (rest low)))]))

; 1String Word -> List-or-words
(check-expect (insert-everywhere/word "a" '())
              (list (list "a")))
(check-expect (insert-everywhere/word "a" (list "b"))
              (list (list "a" "b") (list "b" "a")))
(check-expect
 (insert-everywhere/word "a" (list "b" "c"))
 (list (list "a" "b" "c") (list "b" "a" "c") (list "b" "c" "a")))
(define (insert-everywhere/word s w)
  (cond [(empty? w) (list (cons s '()))]
        [else (cons (cons s w)
                    (insert-at-beginning/in-all-word
                     (first w)
                     (insert-everywhere/word s (rest w))))]))

; 1String List-of-words -> List-of-words
(check-expect (insert-at-beginning/in-all-word "a" '())
              '())
(check-expect (insert-at-beginning/in-all-word "a" (list '()))
              (list (list "a")))
(check-expect (insert-at-beginning/in-all-word "a" (list (list "b")))
              (list (list "a" "b")))
(check-expect
 (insert-at-beginning/in-all-word "a" (list (list "b") (list "c")))
 (list (list "a" "b") (list "a" "c")))
(check-expect
 (insert-at-beginning/in-all-word "a" (list (list "b" "c") (list "c" "b")))
 (list (list "a" "b" "c") (list "a" "c" "b")))
(define (insert-at-beginning/in-all-word s low)
  (cond [(empty? low) '()]
        [else (cons (cons s (first low))
                    (insert-at-beginning/in-all-word s (rest low)))]))
