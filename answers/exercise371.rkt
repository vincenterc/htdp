#lang htdp/isl+

(require 2htdp/abstraction)

; An Xexpr is a list:
; – (cons Symbol Body)
; – (cons Symbol (cons [List-of Attribute] Body))
; where Body is one of:
; - String
; - [List-of Xexpr]
; An Attribute is a list of two items:
;   (cons Symbol (cons String '()))

; <ul>
;   <li>one</li>
;   <li>two</li>
; </ul>
; '(ul (li "one")
;      (li "two"))
