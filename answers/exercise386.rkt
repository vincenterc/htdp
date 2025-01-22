#lang htdp/isl+

(require 2htdp/abstraction)

; An Atom is one of:
; – Number
; – String
; – Symbol

; An Xexpr.v3 is one of:
;  – Symbol
;  – String
;  – Number
;  – (cons Symbol (cons Attribute*.v3 [List-of Xexpr.v3]))
;  – (cons Symbol [List-of Xexpr.v3])
; An Attribute*.v3 is a [List-of Attribute.v3].
; An Attribute.v3 is a list of two items:
;   (list Symbol String)

(define NOT-FOUND "not found")

; Xexpr.v3 String -> String
; retrieves the value of the "content" attribute
; from a 'meta element that has attribute "itemprop"
; with value s
(check-error
 (get '(p "hello") "F")
 NOT-FOUND)
(check-expect
 (get '(meta ((content "+1") (itemprop "F"))) "F")
 "+1")
(check-error
 (get '(meta ((content "+1") (itemprop "F"))) "G")
 NOT-FOUND)
(check-expect
 (get '(head (meta ((content "+1") (itemprop "F")))) "F")
 "+1")
(check-expect
 (get '(html (head (meta ((content "17.09") (itemprop "price")))
                   (meta ((content "+1") (itemprop "F"))))) "F")
 "+1")
(check-error
 (get '(html (head (meta ((content "+1") (itemprop "F"))))) "G")
 NOT-FOUND)
(define (get x s)
  (local ((define result (get-xexpr x s)))
    (if (string? result)
        result
        (error NOT-FOUND))))

; Xexpr.v3 String -> [Maybe String]
; retrieves the value of the "content" attribute
; from a 'meta element that has attribute "itemprop"
; with value s
; produces #false if such 'meta element doesn't exist
(check-expect
 (get-xexpr '(p "hello") "F")
 #false)
(check-expect
 (get-xexpr '(meta ((content "+1") (itemprop "F"))) "F")
 "+1")
(check-expect
 (get-xexpr '(meta ((content "+1") (itemprop "F"))) "G")
 #false)
(check-expect
 (get-xexpr '(head (meta ((content "+1") (itemprop "F")))) "F")
 "+1")
(check-expect
 (get-xexpr '(html (head (meta ((content "17.09") (itemprop "price")))
                         (meta ((content "+1") (itemprop "F"))))) "F")
 "+1")
(check-expect
 (get-xexpr '(html (head (meta ((content "+1") (itemprop "F"))))) "G")
 #false)
(define (get-xexpr xe s)
  (local (; Attribute*.v3
          (define attrs (xexpr-attr xe))
          ; Attribute*.v3 -> [Maybe String]
          (define get-attrs-result
            (if (empty? attrs)
                #false
                (local (; [Maybe String]
                        (define str-with-itemprop
                          (find-attr attrs 'itemprop)))
                  (if (eq? str-with-itemprop s)
                      (find-attr attrs 'content)
                      #false)))))
    (if (string? get-attrs-result)
        get-attrs-result
        (for/or ([a-xe (xexpr-content xe)])
          (get-xexpr a-xe s)))))

; Xexpr.v3 -> Attribute*.v3
; retrieves the list of attributes of xe
(define (xexpr-attr xe)
  (if (atom? xe)
      '()
      (local ((define optional-loa+content (rest xe)))
        (cond
          [(empty? optional-loa+content) '()]
          [else
           (local ((define loa-or-x
                     (first optional-loa+content)))
             (if (list-of-attributes? loa-or-x)
                 loa-or-x
                 '()))]))))

; Xexpr.v3 -> [List-of Xexpr.v3]
; retrieves the list of content elements of xe
(define (xexpr-content xe)
  (if (atom? xe)
      '()
      (local ((define optional-loa+content (rest xe)))
        (cond
          [(empty? optional-loa+content) '()]
          [else
           (if (list-of-attributes? (first optional-loa+content))
               (rest optional-loa+content)
               optional-loa+content)]))))

; Attribute*.v3 or Xexpr.v3 -> Boolean
; is x a list of attributes
(define (list-of-attributes? x)
  (cond
    [(atom? x) #false]
    [(empty? x) #true]
    [else
     (local ((define possible-attribute (first x)))
       (cons? possible-attribute))]))

; Attribute*.v3 Symbol -> [Maybe String]
; retrieves the string which is associated with sy in loa
; #false if no such string
(define (find-attr loa sy)
  (local ((define assq-result (assq sy loa)))
    (if (false? assq-result)
        #false
        (second assq-result))))

; Atom -> Boolean
(define (atom? a)
  (or (number? a)
      (string? a)
      (symbol? a)))
