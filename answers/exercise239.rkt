#lang htdp/isl

; A [List X Y] is a list:
;   (cons X (cons Y '()))

; [List Number Number]
(list 1 2)

; [List Number 1String]
(list 1 "a")

; [List String Boolean]
(list "hello" #true)
