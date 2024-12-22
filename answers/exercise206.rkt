#lang htdp/bsl+

(require 2htdp/itunes)

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
        (list "Play Count" 20) (list "Play Date UTC" date2)))
(define track2
  (list (list "Name" "Only Time") (list "Artist" artist1)
        (list "Album" album1) (list "Total Time" 218096)
        (list "Track Number" 3) (list "Date Added" date3)
        (list "Play Count" 18) (list "Play Date UTC" date4)))
(define track3
  (list (list "Name" "I Still Do") (list "Artist" artist2)
        (list "Album" album2) (list "Total Time" 196780)
        (list "Track Number" 1) (list "Date Added" date5)
        (list "Play Count" 11) (list "Play Date UTC" date6)))

(define ltracks1 '())
(define ltracks2 (list track1 track3))
(define ltracks3 (list track1 track2 track3))

; String LAssoc Any -> Association
; produces the first Association whose first item is equal to key
; from lassoc, or default if there is no such Association
(check-expect (find-association "Artist" '() (list "Artist" ""))
              (list "Artist" ""))
(check-expect (find-association "Artist" track1 (list "Artist" ""))
              (list "Artist" artist1))
(check-expect (find-association "Kind" track1 (list "Kind" ""))
              (list "Kind" ""))
(define (find-association key lassoc default)
  (cond [(empty? lassoc) default]
        [else (if (string=? (first (first lassoc)) key)
                  (first lassoc)
                  (find-association key (rest lassoc) default))]))
