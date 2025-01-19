#lang htdp/isl+

(require 2htdp/image)
(require 2htdp/abstraction)

; An Xexpr is a list:
; – (cons Symbol Body)
; – (cons Symbol (cons [List-of Attribute] Body))
; where Body is short for [List-of Xexpr]
; An Attribute is a list of two items:
;   (cons Symbol (cons String '()))

; An XWord is '(word ((text String)))

; An XItem.v2 is one of:
; – (cons 'li (cons XWord '()))
; – (cons 'li (cons [List-of Attribute] (list XWord)))
; – (cons 'li (cons XEnum.v2 '()))
; – (cons 'li (cons [List-of Attribute] (list XEnum.v2)))
;
; An XEnum.v2 is one of:
; – (cons 'ul [List-of XItem.v2])
; – (cons 'ul (cons [List-of Attribute] [List-of XItem.v2]))

(define e0
  '(ul
    (li (word ((text "hello"))))
    (li (ul ((type "circle"))
            (li (word ((text "hello"))))
            (li (word ((text "world"))))))
    (li (word ((text "world"))))
    (li (word ((text "one"))))))

; XEnum.v2 String String -> XEnum.v2
; replaces all s1 with s2 in xe
(check-expect
 (replace e0 "hello" "bye")
 '(ul
   (li (word ((text "bye"))))
   (li (ul ((type "circle"))
           (li (word ((text "bye"))))
           (li (word ((text "world"))))))
   (li (word ((text "world"))))
   (li (word ((text "one"))))))
(check-expect
 (replace e0 "one" "two")
 '(ul
   (li (word ((text "hello"))))
   (li (ul ((type "circle"))
           (li (word ((text "hello"))))
           (li (word ((text "world"))))))
   (li (word ((text "world"))))
   (li (word ((text "two"))))))
(define (replace xe s1 s2)
  (local (; XItem.v2 -> Number
          ; replaces all s1 with s2 in an-item
          (define (replace-item an-item)
            (local (; XWord or XEnum.v2
                    (define content
                      (first (xexpr-content an-item))))
              (build-xexpr
               (xexpr-name an-item)
               (xexpr-attr an-item)
               (list (cond
                       [(word? content)
                        (if (string=? (word-text content) s1)
                            `(word ((text ,s2)))
                            content)]
                       [else (replace content s1 s2)]))))))
    (build-xexpr (xexpr-name xe)
                 (xexpr-attr xe)
                 (map replace-item (xexpr-content xe)))))

; Symbol [List-of Attribute] [List-of Xexpr] -> Xexpr
; produces the Xexpr given name, attrs and content
(define (build-xexpr name attrs content)
  (cons name
        (if (empty? attrs)
            content
            (cons attrs content))))

; Xexpr -> Symbol
; retrieves the tag of the element of xe
(define (xexpr-name xe) (first xe))

; Xexpr -> [List-of Attribute]
; retrieves the list of attributes of xe
(define (xexpr-attr xe)
  (local ((define optional-loa+content (rest xe)))
    (cond
      [(empty? optional-loa+content) '()]
      [else
       (local ((define loa-or-x
                 (first optional-loa+content)))
         (if (list-of-attributes? loa-or-x)
             loa-or-x
             '()))])))

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
