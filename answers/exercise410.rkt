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

(define schema-0
  `(("Name"    ,string?)
    ("Age"     ,integer?)
    ("Present" ,boolean?)))

(define content-0
  `(("Alice" 35 #true)
    ("Bob"   25 #false)))
(define db-0
  (make-db schema-0
           content-0))

(define content-1
  `(("Carol" 30 #true)
    ("Dave"  32 #false)))
(define db-1
  (make-db schema-0
           content-1))

(define content-2
  `(("Alice" 35 #true)
    ("Bob"   25 #false)
    ("Carol" 30 #true)
    ("Dave"  32 #false)))
(define db-2
  (make-db schema-0
           content-2))

(define content-3
  `(("Bob"   25 #false)
    ("Carol" 30 #true)
    ("Dave"  32 #false)))
(define db-3
  (make-db schema-0
           content-3))

; DB DB -> DB
; produces a DB with the same schema as db1 and db2 and
; the joint content of both
; assume the schemas of db1 and db2 are equal
(check-expect (db-content (db-union db-0 db-1)) content-2)
(check-expect (db-content (db-union db-0 db-3)) content-2)
(define (db-union db1 db2)
  (local ((define schema1 (db-schema db1))
          (define content1 (db-content db1))
          (define content2 (db-content db2))
          ; Content Content -> Content
          (define content-union
            (foldr (lambda (row new-content)
                     (if (member? row new-content)
                         new-content
                         (cons row new-content)))
                   content2
                   content1)))
    (make-db schema1 content-union)))
