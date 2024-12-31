#lang htdp/isl

; [List-of Number] [Number -> Number] -> [List-of Number]
(define (map-n lon f) lon)

; [List-of String] [String -> String] -> [List-of String]
(define (map-s los f) los)

; [X] [List-of X] [X -> X] -> [List-of X]

; [X Y] [List-of X] [X -> Y] -> [List-of Y]
; (define (map1 lox f) loy)
; Y == X
; [X X] [List-of X] [X -> X] -> [List-of X]
