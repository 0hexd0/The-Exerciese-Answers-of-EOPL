#lang racket
;NodeInSequence ::= (Int Listof(Int) Listof(Int))

;number->sequence : Int -> NodeInSequence
(define number->sequence
  (lambda (n)
    (list n '() '())))

;current-element : NodeInSequence -> Int
(define current-element
  (lambda (seq)
    (car seq)))

;get-left : NodeInSequence -> Listof(Int)
(define get-left
  (lambda (seq)
    (cadr seq)))

;get-right : NodeInSequence -> Listof(Int)
(define get-right
  (lambda (seq)
    (caddr seq)))

;move-to-left : NodeInSequence -> NodeInSequence
(define move-to-left
  (lambda (seq)
    (let ([left (get-left seq)]
          [right (get-right seq)])
        (list
     (car left)
     (cdr left)
     (cons
      (current-element seq)
      right)))))

;move-to-right : NodeInSequence -> NodeInSequence
(define move-to-right
  (lambda (seq)
    (let ([left (get-left seq)]
          [right (get-right seq)])
        (list
     (car right)
     (cons
      (current-element seq)
      left)
     (cdr right)))))

;insert-to-left : Int * NodeInSequence -> NodeInSequence
(define insert-to-left
  (lambda (n seq)
    (let ([left (get-left seq)]
          [right (get-right seq)])
      (list
       (current-element seq)
       (cons n left)
       right))))

;insert-to-right : Int * NodeInSequence -> NodeInSequence
(define insert-to-right
  (lambda (n seq)
    (let ([left (get-left seq)]
          [right (get-right seq)])
      (list
       (current-element seq)
       left
       (cons n right)))))

(number->sequence 7)
(current-element '(6 (5 4 3 2 1) (7 8 9)))
(move-to-left '(6 (5 4 3 2 1) (7 8 9)))
(move-to-right '(6 (5 4 3 2 1) (7 8 9)))
(insert-to-left 13 '(6 (5 4 3 2 1) (7 8 9)))
(insert-to-right 13 '(6 (5 4 3 2 1) (7 8 9)))
