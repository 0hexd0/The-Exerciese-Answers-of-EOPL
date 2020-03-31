#lang racket
;⌈⌊val⌋⌉ != val
;set val = (expval->bool true)
;so (expval->num val) = undefined
;so (num-val (expval->num val)) = (num-val undefined) = undefined != true
;so (num-val (expval->num val)) != val