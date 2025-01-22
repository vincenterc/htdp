#lang htdp/isl+

(require 2htdp/batch-io)
(require 2htdp/image)
(require 2htdp/universe)

; An Xexpr.v3 is one of:
;  – Symbol
;  – String
;  – Number
;  – (cons Symbol (cons Attribute*.v3 [List-of Xexpr.v3]))
;  – (cons Symbol [List-of Xexpr.v3])
; An Attribute*.v3 is a [List-of Attribute.v3].
; An Attribute.v3 is a list of two items:
;   (list Symbol String)

(define PREFIX "Https://www.google.com/finance?q=")
(define SIZE 22) ; font size

(define-struct data [price delta])
; A StockWorld is a structure: (make-data String String)
; interpretation (make-data p d)
; p represents the current price of the stock
; d represents the change of the price since the last time
; the price was posted

; String -> StockWorld
; retrieves the stock price of co and its change every 15s
(define (stock-alert co)
  (local ((define url (string-append PREFIX co))
          ; [StockWorld -> StockWorld]
          ; retrieve stock data from url
          (define (retrieve-stock-data __w)
            (local (; [Maybe Xexpr.v3]
                    (define x (read-xexpr/web url)))
              (make-data (get x "price")
                         (get x "priceChange"))))
          ; StockWorld -> Image
          ; produces a text image given w
          (define (render-stock-data w)
            (local (; [StockWorld -> String] String -> Image
                    ; produces a text image given a selector sel
                    ; and a color col
                    (define (word sel col)
                      (text (sel w) SIZE col)))
              (overlay (beside (word data-price 'black)
                               (text "  " SIZE 'white)
                               (word data-delta 'red))
                       (rectangle 300 35 'solid 'white)))))
    (big-bang (retrieve-stock-data 'no-use)
      [on-tick retrieve-stock-data 15]
      [to-draw render-stock-data])))

; [Maybe Xexpr.v3] String -> String
; a fake get function
(define (get xe s) "")
