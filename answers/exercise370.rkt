#lang htdp/isl+

(require 2htdp/abstraction)

; An Xexpr is a list:
; – (cons Symbol Body)
; – (cons Symbol (cons [List-of Attribute] Body))
; where Body is short for [List-of Xexpr]
; An Attribute is a list of two items:
;   (cons Symbol (cons String '()))

; An XWord is '(word ((text String)))

(define xword0 '(word ((text "hello"))))
(define xword1 '(word ((text "world"))))
(define xword2 '(word ((text "racket"))))

; Any -> Boolean
; determines whether v is in XWord
(check-expect (word? xword0) #true)
(check-expect (word? xword0) #true)
(check-expect (word? xword0) #true)
(check-expect (word? 'a) #false)
(check-expect (word? '(machine (action) (action))) #false)
(check-expect (word? '("word" ((text "hello")))) #false)
(check-expect (word? '(machine ((text "hello")))) #false)
(check-expect (word? '(word hello)) #false)
(check-expect (word? '(word ((text "hello") (name "sam")))) #false)
(check-expect (word? '(word (a))) #false)
(check-expect (word? '(word ((text)))) #false)
(check-expect (word? '(word (("text" "hello")))) #false)
(check-expect (word? '(word ((name "sam")))) #false)
(check-expect (word? '(word ((test hello)))) #false)
(define (word? v)
  (match v
    [(list 'word (list (list 'text str)))
     (string? str)]
    [_ #false]))
; v1
; (define (word? v)
;   (and
;    (cons? v)
;    (= (length v) 2)
;    (local
;      ((define maybe-tag (first v))
;       (define maybe-attrs (second v)))
;      (and
;       (symbol? maybe-tag)
;       (symbol=? maybe-tag 'word)
;       (cons? maybe-attrs)
;       (= (length maybe-attrs) 1)
;       (local
;         ((define maybe-attr (first maybe-attrs)))
;         (and (cons? maybe-attr)
;              (= (length maybe-attr) 2)
;              (symbol? (first maybe-attr))
;              (symbol=? (first maybe-attr) 'text)
;              (string? (second maybe-attr))))))))

; XWord -> String
; extracts the value of the only attribute of xw
(check-expect (word-text xword0) "hello")
(check-expect (word-text xword1) "world")
(check-expect (word-text xword2) "racket")
(define (word-text xw)
  (second (first (second xw))))
