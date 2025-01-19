#lang htdp/isl+

; An Xexpr.v0 (short for X-expression) is a one-item list:
;   (cons Symbol '())

; An Xexpr.v1 is a list:
;   (cons Symbol [List-of Xexpr.v1])

; An Xexpr.v2 is a list:
; – (cons Symbol Body)
; – (cons Symbol (cons [List-of Attribute] Body))
; where Body is short for [List-of Xexpr.v2]
; An Attribute is a list of two items:
;   (cons Symbol (cons String '()))

; '(server ((name "example.org")))
; <server name="example.org" />

; '(carcas (board (grass)) (player ((name "sam"))))
; <carcas><board><grass /></board><player name="sam" />></carcas>

; '(start)
; <start />
; This one is also an element of Xexpr.v0 and Xexpr.v1