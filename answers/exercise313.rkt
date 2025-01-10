#lang htdp/isl+

(define-struct no-parent [])
(define NP (make-no-parent))
(define-struct child [father mother name date eyes])
; An FT (short for family tree) is one of:
; – NP
; – (make-child FT FT String N String)

; Oldest Generation:
(define Carl (make-child NP NP "Carl" 1926 "green"))
(define Bettina (make-child NP NP "Bettina" 1926 "green"))

; Middle Generation:
(define Adam (make-child Carl Bettina "Adam" 1950 "hazel"))
(define Dave (make-child Carl Bettina "Dave" 1955 "black"))
(define Eva (make-child Carl Bettina "Eva" 1965 "blue"))
(define Fred (make-child NP NP "Fred" 1966 "pink"))

; Youngest Generation:
(define Gustav (make-child Fred Eva "Gustav" 1988 "brown"))

; FT -> Boolean
; is there any ancestor has blue eyes in the tree an-ftree
(check-expect (blue-eyed-ancestor? Eva) #false)
(check-expect (blue-eyed-ancestor? Gustav) #true)
(define (blue-eyed-ancestor? an-ftree)
  (cond
    [(no-parent? an-ftree) #false]
    [else
     (or (blue-eyed? (child-father an-ftree))
         (blue-eyed? (child-mother an-ftree))
         (blue-eyed-ancestor?
          (child-father an-ftree))
         (blue-eyed-ancestor?
          (child-mother an-ftree)))]))

; FT -> Boolean
; is the given tree an-ftree a child structure
; does this child structure have "blue" in the eyes filed
(define (blue-eyed? an-ftree)
  (and (child? an-ftree)
       (string=? (child-eyes an-ftree) "blue")))

; Wrong version!!
; It always produces #false no matter which an-ftree is chosen.
; (define (blue-eyed-ancestor? an-ftree)
;   (cond
;     [(no-parent? an-ftree) #false]
;     [else
;      (or
;       (blue-eyed-ancestor?
;        (child-father an-ftree))
;       (blue-eyed-ancestor?
;        (child-mother an-ftree)))]))
