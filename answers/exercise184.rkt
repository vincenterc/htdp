#lang htdp/bsl+

(check-expect
 (list #false #false)
 (list (string=? "a" "b") #false))

(check-expect
 (list 30 200 0.5)
 (list (+ 10 20) (* 10 20) (/ 10 20)))

(check-expect
 (list "dana" "jane" "mary" "laura")
 (list "dana" "jane" "mary" "laura"))
