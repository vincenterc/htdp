#lang htdp/bsl

(require 2htdp/image)

; An Lo1S is one of:
; – '()
; – (cons 1String Lo1S)

(define FONT-SIZE 16) ; the font size
(define FONT-COLOR "black") ; the font color

; Lo1s -> Image
; renders a list of 1Strings as a text image
(check-expect
 (editor-text
  (cons "p" (cons "o" (cons "s" (cons "t" '())))))
 (text "post" FONT-SIZE FONT-COLOR))
(check-expect
 (editor-text '())
 (text "" FONT-SIZE FONT-COLOR))
(define (editor-text s)
  (text (lo1s->string s) FONT-SIZE FONT-COLOR))

; Lo1s -> String
; converts a list of 1Strings to a string
(check-expect (lo1s->string '()) "")
(check-expect
 (lo1s->string
  (cons "p" (cons "o" (cons "s" (cons "t" '())))))
 "post")
(define (lo1s->string l)
  (cond [(empty? l) ""]
        [else (string-append
               (first l)
               (lo1s->string (rest l)))]))
