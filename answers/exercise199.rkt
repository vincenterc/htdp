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

(define track1
  (create-track "Wild Child" "Enya" album1
                227996 2 date1 20 date2))
(define track2
  (create-track "Only Time" "Enya" album1
                218096 3 date3 18 date4))
(define track3
  (create-track "I Still Do" "The Cranberries" album2
                196780 1 date5 11 date6))

(define ltracks1 '())
(define ltracks2 (list track1 track3))
(define ltracks3 (list track1 track2 track3))
