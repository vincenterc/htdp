#lang htdp/isl+

(require 2htdp/image)
(require 2htdp/abstraction)

; An Xexpr is a list:
; – (cons Symbol Body)
; – (cons Symbol (cons [List-of Attribute] Body))
; where Body is short for [List-of Xexpr]
; An Attribute is a list of two items:
;   (cons Symbol (cons String '()))

; An XItem.v2 is one of:
; – (cons 'li (cons XWord '()))
; – (cons 'li (cons [List-of Attribute] (cons XWord '())))
; – (cons 'li (cons XEnum.v2 '()))
; – (cons 'li (cons [List-of Attribute] (cons XEnum.v2 '())))
;
; An XEnum.v2 is one of:
; – (cons 'ul [List-of XItem.v2])
; – (cons 'ul (cons [List-of Attribute] [List-of XItem.v2]))

(define i0 '(li (word ((text "one")))))
(define i1 '(li (ul
                 (li (word ((text "one"))))
                 (li (word ((text "two")))))))

(define e0
  '(ul
    (li (word ((text "one"))))
    (li (ul
         (li (word ((text "two"))))
         (li (word ((text "three"))))))))

(define SIZE 12)
(define COLOR "black")
(define BT
  (beside (circle 1 "solid" "black") (text " " SIZE COLOR)))

; Image -> Image
; marks item with bullet
(define (bulletize item)
  (beside/align 'center BT item))

; XEnum.v2 -> Image
; renders an XEnum.v2 as an image
(check-expect (render-enum e0)
              (above/align
               'left
               (bulletize (text "one" SIZE COLOR))
               (bulletize
                (above/align
                 'left
                 (bulletize (text "two" SIZE COLOR))
                 (bulletize (text "three" SIZE COLOR))))))
(define (render-enum xe)
  (local ((define content (xexpr-content xe))
          ; XItem.v2 Image -> Image
          (define (deal-with-one item so-far)
            (above/align 'left (render-item item) so-far)))
    (foldr deal-with-one empty-image content)))

; XItem.v2 -> Image
; renders one XItem.v2 as an image
(check-expect (render-item i0)
              (bulletize (text "one" SIZE COLOR)))
(check-expect (render-item i1)
              (bulletize
               (above/align
                'left
                (bulletize (text "one" SIZE COLOR))
                (bulletize (text "two" SIZE COLOR))
                empty-image)))
(define (render-item an-item)
  (local ((define content (first (xexpr-content an-item))))
    (bulletize
     (cond [(word? content)
            (text (word-text content) SIZE COLOR)]
           [else (render-enum content)]))))

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

; Any -> Boolean
; determines whether v is in XWord
(define (word? v)
  (match v
    [(list 'word (list (list 'text str)))
     (string? str)]
    [_ #false]))

; XWord -> String
; extracts the value of the only attribute of xw
(define (word-text xw)
  (second (first (second xw))))
