#lang racket
;⌈⌊val⌋⌉ != val
;set val = (bool-val true)
;so (expval->num val) = undefined
;so (num-val (expval->num val)) = (num-val undefined) = undefined != (bool-val true)
;so (num-val (expval->num val)) != val