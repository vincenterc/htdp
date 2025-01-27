#lang htdp/isl+

(define-struct db [schema content])
; A DB is a structure: (make-db Schema Content)

; A Schema is a [List-of Spec]
; A Spec is a [List Label Predicate]
; A Label is a String
; A Predicate is a [Any -> Boolean]

; A (piece of) Content is a [List-of Row]
; A Row is a [List-of Cell]
; A Cell is Any
; constraint cells do not contain functions

; integrity constraint In (make-db sch con),
; for every row in con,
; (I1) its length is the same as sch's, and
; (I2) its ith Cell satisfies the ith Predicate in sch

(define school-schema
  `(("Name"    ,string?)
    ("Age"     ,integer?)
    ("Present" ,boolean?)))
(define school-content
  `(("Alice" 35 #true)
    ("Bob"   25 #false)
    ("Carol" 30 #true)
    ("Dave"  32 #false)))
(define school-db
  (make-db school-schema
           school-content))

(define select-presence-content
  `(("Alice" #true)
    ("Carol" #true)))

(define select-older-30-content
  `(("Alice" 35)
    ("Dave"  32)))

; DB [List-of Label] [Row -> Boolean] -> Content
; produces a list of rows that satisfy the predicate p,
; projected down to the set of labels, from db
(check-expect
 (select school-db
         '("Name" "Present")
         (lambda (row) (third row)))
 select-presence-content)
(check-expect
 (select school-db
         '("Name" "Age")
         (lambda (row) (> (second row) 30)))
 select-older-30-content)
(define (select db labels p)
  (local ((define schema (db-schema db))
          (define content (db-content db))
          (define content-filtered (filter p content))

          ; Spec -> Boolean
          ; does this column belong to the new schema
          (define (keep? c)
            (member? (first c) labels))

          ; Row -> Row
          ; retains those columns whose name is in labels
          (define (row-project row)
            (foldr (lambda (cell m c) (if m (cons cell c) c))
                   '()
                   row
                   mask))
          (define mask (map keep? schema)))
    (map row-project content-filtered)))
