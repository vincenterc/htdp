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

; String Date LTracks -> LTracks
; produces the list of tracks that belong to the given album a
; and have been played after the given date d from lt
(check-expect
 (select-album-date
  album1 (create-date 2011 1 1 0 0 0) ltracks1)
 '())
(check-expect
 (select-album-date
  album1 (create-date 2011 1 1 0 0 0) ltracks2)
 (list track1))
(check-expect
 (select-album-date
  "Wintersong" (create-date 2011 1 1 0 0 0) ltracks2)
 '())
(check-expect
 (select-album-date
  album2
  (create-date 2011 1 1 0 0 0) ltracks3)
 '())
(check-expect
 (select-album-date
  album2
  (create-date 2010 1 1 0 0 0) ltracks3)
 (list track3))
(define (select-album-date a d lt)
  (cond [(empty? lt) '()]
        [else (if (and (string=? (track-album (first lt)) a)
                       (date<? d (track-played (first lt))))
                  (cons (first lt) (select-album-date a d (rest lt)))
                  (select-album-date a d (rest lt)))]))

; Date Date -> Boolean
; determines whether the first date occurs before the second
(check-expect (date<? (create-date 2010 10 10 10 10 10)
                      (create-date 2011 10 10 10 10 10))
              #true)
(check-expect (date<? (create-date 2010 10 10 10 10 10)
                      (create-date 2009 11 10 10 10 10))
              #false)
(check-expect (date<? (create-date 2010 10 10 10 10 10)
                      (create-date 2010 11 10 10 10 10))
              #true)
(check-expect (date<? (create-date 2010 10 10 10 10 10)
                      (create-date 2010  9 11 10 10 10))
              #false)
(check-expect (date<? (create-date 2010 10 10 10 10 10)
                      (create-date 2010 10 11 10 10 10))
              #true)
(check-expect (date<? (create-date 2010 10 10 10 10 10)
                      (create-date 2010 10  9 11 10 10))
              #false)
(check-expect (date<? (create-date 2010 10 10 10 10 10)
                      (create-date 2010 10 10 11 10 10))
              #true)
(check-expect (date<? (create-date 2010 10 10 10 10 10)
                      (create-date 2010 10 10  9 11 10))
              #false)
(check-expect (date<? (create-date 2010 10 10 10 10 10)
                      (create-date 2010 10 10 10 11 10))
              #true)
(check-expect (date<? (create-date 2010 10 10 10 10 10)
                      (create-date 2010 10 10 10  9 11))
              #false)
(check-expect (date<? (create-date 2010 10 10 10 10 10)
                      (create-date 2010 10 10 10 10 11))
              #true)
(check-expect (date<? (create-date 2010 10 10 10 10 10)
                      (create-date 2010 10 10 10 10  9))
              #false)
(define (date<? d1 d2)
  (cond
    [(< (date-year d1) (date-year d2)) #true]
    [(> (date-year d1) (date-year d2)) #false]
    [(< (date-month d1) (date-month d2)) #true]
    [(> (date-month d1) (date-month d2)) #false]
    [(< (date-day d1) (date-day d2)) #true]
    [(> (date-day d1) (date-day d2)) #false]
    [(< (date-hour d1) (date-hour d2)) #true]
    [(> (date-hour d1) (date-hour d2)) #false]
    [(< (date-minute d1) (date-minute d2)) #true]
    [(> (date-minute d1) (date-minute d2)) #false]
    [(< (date-second d1) (date-second d2)) #true]
    [(> (date-second d1) (date-second d2)) #false]
    [else #false]))

; Application
; (define ITUNES-LOCATION "./files/itunes.xml")
; ; LTracks
; (define itunes-tracks
;   (read-itunes-as-tracks ITUNES-LOCATION))

; (select-album-date album1
;                    (create-date 2011 1 1 0 0 0)
;                    itunes-tracks)
