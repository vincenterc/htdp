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
    (li (ul
         (li (word ((text "hello"))))
         (li (word ((text "world"))))))
    (li (word ((text "world"))))
    (li (word ((text "one"))))))

; XEnum.v2 String -> Number
; counts the number of s in xe
(check-expect (count e0 "hello") 2)
(check-expect (count e0 "world") 2)
(check-expect (count e0 "one") 1)
(check-expect (count e0 "two") 0)
(define (count xe s)
  (local (; [List-of XEnum.v2]
          (define content (xexpr-content xe))
          ; XItem.v2 -> Number
          ; counts the number of s in an-item
          (define (count-item an-item)
            (local (; XWord or XEnum.v2
                    (define content
                      (first (xexpr-content an-item))))
              (cond
                [(word? content)
                 (if (string=? (word-text content) s) 1 0)]
                [else (count content s)]))))
    (for/sum ([item content]) (count-item item))))
; v1
; (define (count xe s)
;   (local (; [List-of XEnum.v2]
;           (define content (xexpr-content xe))
;           ; XItem.v2 -> Number
;           ; counts the number of s in an-item
;           (define (count-item an-item)
;             (local (; XWord or XEnum.v2
;                     (define content
;                       (first (xexpr-content an-item))))
;               (cond
;                 [(word? content)
;                  (if (string=? (word-text content) s) 1 0)]
;                 [else (count content s)]))))
;     (foldl
;      (lambda (item so-far)
;        (+ (count-item item) so-far))
;      0
;      content)))

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
