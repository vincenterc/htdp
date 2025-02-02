#lang htdp/isl+

; A Line is a [List-of 1String].

; A Token is one of:
; - 1String (without " " "\t" "\n" "\r")
; - String that consists of lower-case letters

(define LETTERS
  (explode "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"))

; Line -> [List-of Token]
; turns a Line into a list of Tokens
(check-expect (tokenize '()) '())
(check-expect
 (tokenize (list "H" "o" "w" " " "a" "r" "e" " " "U" "?"))
 '("how" "are" "U" "?"))
(check-expect
 (tokenize (list "\\" "a" "n" "y" "\t" " " " " "w" "o" "r" "d"))
 '("\\" "any" "word"))
(define (tokenize line)
  (cond
    [(empty? line) '()]
    [(string-whitespace? (first line))
     (tokenize (rest line))]
    [(not (member? (first line) LETTERS))
     (cons (first line) (tokenize (rest line)))]
    [else
     (cons (first-letter-token line)
           (tokenize (remove-first-letter-token line)))]))

; Line -> Token
; retrieves the prefix of aline up to the first occurrence of
; the non-letter 1String
(define (first-letter-token aline)
  (local
    (; Line -> String
     ; retrieves the prefix of l up to the first occurrence of
     ; the non-letter 1String
     (define (first-letter-token-aux l)
       (cond
         [(empty? l) '()]
         [(not (member? (first l) LETTERS)) '()]
         [else
          (cons
           (first l)
           (first-letter-token-aux (rest l)))]))
     (define str (implode (first-letter-token-aux aline))))
    (if (= (string-length str) 1)
        str
        (string-downcase str))))

; Line -> Line
; retrieves the prefix of aline up to the first occurrence of
; the non-letter 1String
(define (remove-first-letter-token aline)
  (cond
    [(empty? aline) '()]
    [(not (member? (first aline) LETTERS)) aline]
    [else (remove-first-letter-token (rest aline))]))
