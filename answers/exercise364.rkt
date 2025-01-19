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

; <transition from="seen-e" to="seen-f" />
; '(transition ((from "seen-e") (to "seen-f")))

; <ul><li><word /><word /></li><li><word /></li></ul>
; '(ul (li (word) (word)) (li (word)))
; This one could also be represented in Xexpr.v1
