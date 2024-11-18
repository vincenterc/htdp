#lang htdp/bsl

(define (==> sunny friday)
  (or (not sunny) friday))

(boolean=? (==> #false #true) #true)
