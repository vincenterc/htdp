#lang htdp/bsl+

(require 2htdp/itunes)

; A List-of-strings is one of:
; – '()
; – (cons String List-of-strings)

(define date1
  (create-date 2002 7 17 3 55 14))
(define date2
  (create-date 2011 5 17 17 35 13))
(define date3
  (create-date 2002 7 17 3 55 42))
(define date4
  (create-date 2011 5 17 17 38 47))
(define date5
  (create-date 2002 7 18 1 2 25))
(define date6
  (create-date 2010 2 14 17 25 20))

(define album1 "A Day Without Rain")
(define album2 "Everybody Else Is Doing It, So Why Can't We?")

(define artist1 "Enya")
(define artist2 "The Cranberries")

(define track1
  (list (list "Name" "Wild Child") (list "Artist" artist1)
        (list "Album" album1) (list "Total Time" 227996)
        (list "Track Number" 2) (list "Date Added" date1)
        (list "Play Count" 20) (list "Play Date UTC" date2)
        (list "Purchased" #true)))
(define track2
  (list (list "Name" "Only Time") (list "Artist" artist1)
        (list "Album" album1) (list "Total Time" 218096)
        (list "Track Number" 3) (list "Date Added" date3)
        (list "Play Count" 18) (list "Play Date UTC" date4)
        (list "Purchased" #true)))
(define track3
  (list (list "Name" "I Still Do") (list "Artist" artist2)
        (list "Album" album2) (list "Total Time" 196780)
        (list "Track Number" 1) (list "Date Added" date5)
        (list "Play Count" 11) (list "Play Date UTC" date6)))

(define ltracks1 '())
(define ltracks2 (list track1 track3))
(define ltracks3 (list track1 track2 track3))

; LLists -> List-of-strings
; produces a list of strings that are associated with a Boolean attribute
; given ll
(check-expect (boolean-attributes '()) '())
(check-expect (boolean-attributes
               (list (list (list "Name" "a1")) (list (list "Name" "a2"))))
              '())
(check-expect (boolean-attributes
               (list (list (list "Name" "a1"))
                     (list (list "Name" "a2"))
                     (list (list "Name" "b1") (list "Purchased" #true))
                     (list (list "Name" "b2") (list "Purchased" #true))))
              (list "Purchased"))
(check-expect (boolean-attributes
               (list (list (list "Name" "a1"))
                     (list (list "Name" "a2"))
                     (list (list "Name" "b1") (list "Purchased" #true))
                     (list (list "Name" "b2") (list "Purchased" #true))
                     (list (list "Name" "c1"))
                     (list (list "Name" "c2") (list "Disabled" #true))))
              (list "Purchased" "Disabled"))
(check-expect (boolean-attributes ltracks3) (list "Purchased"))
(define (boolean-attributes ll)
  (create-set
   (cond [(empty? ll) '()]
         [else (append (boolean-attributes/lassoc (first ll))
                       (boolean-attributes (rest ll)))])))

; LAssoc -> List-of-string
; produces a list of strings that are associated with a Boolean attribute
; given lassoc
(check-expect (boolean-attributes/lassoc '()) '())
(check-expect (boolean-attributes/lassoc (list (list "Name" "a"))) '())
(check-expect (boolean-attributes/lassoc
               (list (list "Name" "a") (list "Purchased" #true)))
              (list "Purchased"))
(define (boolean-attributes/lassoc lassoc)
  (cond [(empty? lassoc) '()]
        [else (if (boolean? (second (first lassoc)))
                  (cons (first (first lassoc))
                        (boolean-attributes/lassoc (rest lassoc)))
                  (boolean-attributes/lassoc (rest lassoc)))]))

; List-of-strings -> List-of-strings
; produces the list of strings that contains every String
; from the given list los exactly once
(check-expect (create-set '()) '())
(check-expect (create-set (list "a"))
              (list "a"))
(check-expect (create-set (list "a" "b"))
              (list "a" "b"))
(check-expect (create-set (list "a" "a" "b"))
              (list "a" "b"))
(define (create-set los)
  (cond [(empty? los) '()]
        [else (cons-or-not (first los) (create-set (rest los)))]))

; String -> List-of-strings
; constructs a new list only when s is not a member of los,
; otherwise returns los
(check-expect (cons-or-not "a" (list "a" "b"))
              (list "a" "b"))
(check-expect (cons-or-not "a" (list "b" "c"))
              (list "a" "b" "c"))
(define (cons-or-not s los)
  (if (member? s los)
      los
      (cons s los)))

; Application
; (define ITUNES-LOCATION "./files/itunes.xml")
; ; LLists
; (define list-tracks
;   (read-itunes-as-lists ITUNES-LOCATION))

; (boolean-attributes list-tracks)
