#lang htdp/isl+

; An Xexpr.v2 is a list:
;   (cons Symbol Body)
; where Body is one of:
; - [List-of Xexpr.v2]
; - (cons [List-of Attribute] [List-of Xexpr.v2])
; An Attribute is a list of two items:
;   (cons Symbol (cons String '()))
