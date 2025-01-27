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

(define reorder-schema-0
  `(("Name"    ,string?)
    ("Present" ,boolean?)
    ("Age"     ,integer?)))
(define reorder-content-0
  `(("Alice" #true  35)
    ("Bob"   #false 25)
    ("Carol" #true  30)
    ("Dave"  #false 32)))
(define reorder-db-0
  (make-db reorder-schema-0
           reorder-content-0))

(define reorder-schema-1
  `(("Age"  ,integer?)
    ("Name" ,string?)))
(define reorder-content-1
  `((35 "Alice")
    (25 "Bob")
    (30 "Carol")
    (32 "Dave")))
(define reorder-db-1
  (make-db reorder-schema-1
           reorder-content-1))

(define reorder-schema-2
  `(("Name"    ,string?)
    ("Age"     ,integer?)))
(define reorder-content-2
  `(("Alice" 35)
    ("Bob"   25)
    ("Carol" 30)
    ("Dave"  32)))
(define reorder-db-2
  (make-db reorder-schema-2
           reorder-content-2))

; DB [List-of Label] -> DB
; reorders the columns of db according to lol
(check-expect
 (db-content (reorder school-db '("Name" "Present" "Age")))
 reorder-content-0)
(check-expect
 (db-content (reorder school-db '("Age" "Name")))
 reorder-content-1)
(check-expect
 (db-content (reorder school-db '("Name" "Description" "Age")))
 reorder-content-2)
(define (reorder db lol)
  (local ((define schema (db-schema db))
          (define labels (map first schema))
          (define content (db-content db))
          ; [List-of [Maybe N]]
          ; the indices of lol in labels
          (define indices-of-lol
            (map (lambda (l) (index-of labels l)) lol))
          ; [X] [List-of X] -> [N [List-of X] -> [List-of X]]
          ; reorders old-list according indices-of-lol
          (define (reorder-list old-list)
            (foldr (lambda (index new-list)
                     (if (boolean? index)
                         new-list
                         (cons (list-ref old-list index) new-list)))
                   '()
                   indices-of-lol))
          (define schema-reordered (reorder-list schema))
          (define content-reordered (map reorder-list content)))
    (make-db schema-reordered
             content-reordered)))

; [X] [List-of X] X -> [Maybe N]
; finds the index of v in l
; #false if v does not exist
(check-expect (index-of '() 3) #false)
(check-expect (index-of '(1 2 3 4) 3) 2)
(check-expect (index-of '(1 2 3 4) 5) #false)
(define (index-of l v)
  (cond
    [(empty? l) #false]
    [else
     (if (equal? (first l) v)
         0
         (local (; [Maybe N]
                 (define index (index-of (rest l) v)))
           (if (boolean? index)
               #false
               (add1 index))))]))
