#lang htdp/isl+

(require 2htdp/image)

; An Xexpr is a list:
; – (cons Symbol Body)
; – (cons Symbol (cons Attributes Body))
; where Body is short for [List-of Xexpr]
; An Attributes is [List-of Attribute]
; An Attribute is a list of two items:
;   (cons Symbol (cons String '()))

; An XWord is '(word ((text String)))

; An XEnum.v1 is one of:
; – (cons 'ul [List-of XItem.v1])
; – (cons 'ul (cons Attributes [List-of XItem.v1]))
; An XItem.v1 is one of:
; – (cons 'li (cons XWord '()))
; – (cons 'li (cons Attributes (cons XWord '())))

(define i1 '(li (word ((text "one")))))
(define i2 '(li (word ((text "two")))))

(define BT (circle 3 "solid" "black"))

; XItem.v1 -> Image
; renders an item as a "word" prefixed by a bullet
(check-expect
 (render-item1 i1)
 (beside/align 'center BT (text "one" 12 'black)))
(check-expect
 (render-item1 i2)
 (beside/align 'center BT (text "two" 12 'black)))
(define (render-item1 i)
  (local ((define content (xexpr-content i))
          (define element (first content))
          (define a-word (word-text element))
          (define item (text a-word 12 'black)))
    (beside/align 'center BT item)))

; Xexpr -> [List-of Xexpr]
; retrieves the list of content elements of xe
(define (xexpr-content xe)
  (local ((define optional-loa+content (rest xe)))
    (cond
      [(empty? optional-loa+content) '()]
      [else
       (if (list-of-attributes? (first optional-loa+content))
           (rest optional-loa+content)
           optional-loa+content)])))

; [List-of Attribute] or Xexpr -> Boolean
; is x a list of attributes
(define (list-of-attributes? x)
  (cond
    [(empty? x) #true]
    [else
     (local ((define possible-attribute (first x)))
       (cons? possible-attribute))]))

; XWord -> String
; extracts the value of the only attribute of xw
(define (word-text xw)
  (second (first (second xw))))
