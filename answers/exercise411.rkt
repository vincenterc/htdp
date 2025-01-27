#lang htdp/isl+

(require 2htdp/abstraction)

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
    ("Bob"   25 #false)
    ("Carol" 30 #true)
    ("Dave"  32 #false)))
(define db-0
  (make-db schema-0
           content-0))

(define schema-1
  `(("Present"     ,boolean?)
    ("Description" ,string?)
    ("Description-jp" ,string?)))
(define content-1
  `((#true  "presence" "here")
    (#false "absence" "there")))
(define db-1
  (make-db schema-1
           content-1))

(define schema-2
  `(("Name"           ,string?)
    ("Age"            ,integer?)
    ("Description"    ,string?)
    ("Description-2" ,string?)))
(define content-2
  `(("Alice" 35 "presence" "here")
    ("Bob"   25 "absence" "there")
    ("Carol" 30 "presence" "here")
    ("Dave"  32 "absence" "there")))
(define db-2
  (make-db schema-2
           content-2))

(define schema-3
  `(("Present"     ,boolean?)
    ("Description" ,string?)))
(define content-3
  `((#true  "presence")
    (#true  "here")
    (#false "absence")
    (#false "there")))
(define db-3
  (make-db schema-3
           content-3))

(define schema-4
  `(("Name"    ,string?)
    ("Age"     ,integer?)
    ("Present" ,boolean?)))
(define content-4
  `(("Alice" 35 "presence")
    ("Alice" 35 "here")
    ("Bob"   25 "absence")
    ("Bob"   25 "there")
    ("Carol" 30 "presence")
    ("Carol" 30 "here")
    ("Dave"  32 "absence")
    ("Dave"  32 "there")))
(define db-4
  (make-db schema-4
           content-4))

; DB DB -> DB
; produces a database from db1 by replacing the last cell in each row
; with the translation of the cell in db-2
; assume the first spec of the schema of db2 is equal to
; the last spec of db1
(check-expect
 (db-content (join db-0 db-1))
 content-2)
(check-expect
 (db-content (join db-0 db-3))
 content-4)
(define (join db1 db2)
  (local ((define schema1 (db-schema db1))
          (define schema2 (db-schema db2))
          (define content1 (db-content db1))
          (define content2 (db-content db2))
          ; [List-of Any] [List-of Any] -> [List-of Any]
          ; joins two lists
          ; assume the last item of l1 is equal to the first item of l2
          (define (join-lists l1 l2)
            (cond [(empty? (rest l1)) l2]
                  (else (cons (first l1) (join-lists (rest l1) l2)))))
          (define schema-joined (join-lists schema1 schema2))
          (define content-joined
            (for*/list
                ([row content1]
                 [row-to-join
                  ; content to join
                  (map rest
                       ; content2 filtered
                       (filter (lambda (r)
                                 (eq? (first r)
                                      ; last item of row
                                      (list-ref row (sub1 (length row)))))
                               content2))])
              (join-lists row row-to-join))))
    (make-db schema-joined content-joined)))
