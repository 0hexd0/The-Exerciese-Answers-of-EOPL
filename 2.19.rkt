#lang racket
;Bintree ::= () | (Int Bintree Bintree)

;number->bintree : Int -> Bintree
(define number->bintree
  (lambda (n)
    (list n '() '())))

;at-leaf? : Bintree -> Bool
(define at-leaf?
  (lambda (tree)
    (null? tree)))

;current-element : Bintree -> Int | ()
(define current-element
  (lambda (tree)
    (if (at-leaf? tree)
        '()
        (car tree))))

;move-to-left : Bintree -> Bintree
(define move-to-left
  (lambda (tree)
    (if (at-leaf? tree)
        '()
        (cadr tree))))

;move-to-right : Bintree -> Bintree
(define move-to-right
  (lambda (tree)
    (if (at-leaf? tree)
        '()
        (caddr tree))))

;insert-to-left : Int * Bintree -> Bintree
(define insert-to-left
  (lambda (n tree)
    (let ([left (move-to-left tree)]
          [right (move-to-right tree)])
      (list
       (current-element tree)
       (list n
             left
             '())
       right))))

;insert-to-right : Int * Bintree -> Bintree
(define insert-to-right
  (lambda (n tree)
    (let ([left (move-to-left tree)]
          [right (move-to-right tree)])
      (list
       (current-element tree)
       left
       (list n
             '()
             right)))))

(number->bintree 13)

(define t1 (insert-to-right 14
                            (insert-to-left 12
                                            (number->bintree 13))))
t1
(move-to-left t1)
(current-element (move-to-left t1))
(at-leaf? (move-to-right (move-to-left t1)))
(insert-to-left 15 t1)
