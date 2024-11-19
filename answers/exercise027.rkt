#lang htdp/bsl

(define BASE-PRICE 5.0)
(define BASE-ATTENDEES 120)

(define PRICE-CHANGE 0.1)
(define ATTENDEE-CHANGE 15)

(define PERFORMANCE-COST 180)
(define ATTENDEE-COST 0.04)

(define (attendees ticket-price)
  (- BASE-ATTENDEES (* (- ticket-price BASE-PRICE) (/ ATTENDEE-CHANGE PRICE-CHANGE))))

(define (revenue ticket-price)
  (* ticket-price (attendees ticket-price)))

(define (cost ticket-price)
  (+ PERFORMANCE-COST (* ATTENDEE-COST (attendees ticket-price))))

(define (profit ticket-price)
  (- (revenue ticket-price)
     (cost ticket-price)))

(profit 1) ; 511.2
(profit 2) ; 937.2
(profit 3) ; 1063.2
(profit 4) ; 889.2
(profit 5) ; 415.2
