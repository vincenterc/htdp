#lang htdp/isl

; An NEList-of-temperatures is one of:
; – (cons CTemperature '())
; – (cons CTemperature NEList-of-temperatures)

; An NEList-of-Booleans is one of:
; – (cons Boolean '())
; – (cons Boolean NEList-of-Booleans)

; A [NEList-of ITEM] is one of:
; - (cons ITEM '())
; - (cons ITEM [NEList-of ITEM])
