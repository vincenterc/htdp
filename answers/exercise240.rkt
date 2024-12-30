#lang htdp/isl

(define-struct layer [stuff])

; An LStr is one of:
; – String
; – (make-layer LStr)
(define lstr-1 "hello")
(define lstr-2 (make-layer "hello"))
(define lstr-3 (make-layer (make-layer "hello")))

; An LNum is one of:
; – Number
; – (make-layer LNum)
(define lnum-1 1)
(define lnum-2 (make-layer 1))
(define lnum-3 (make-layer (make-layer 1)))

; A [Layer S] is one of:
; - S
; - (make-layer [Layer S])

; [Layer String]
; [Layer Number]
