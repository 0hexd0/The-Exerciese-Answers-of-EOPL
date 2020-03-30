#lang racket
(define empty-stack
  (lambda ()
    (lambda (type)
      (if (eqv? type 'top)
          null
          null))))

(define top
  (lambda (stack)
    (stack 'top)))

(define push
  (lambda (stack val)
    (lambda (type)
      (if (eqv? type 'top)
          val
          stack))))

(define pop
  (lambda (stack)
    (stack 'pop)))

(define empty-stack?
  (lambda (stack)
    (null? (top stack))))

(empty-stack? (push (empty-stack) 1))

(top
 (pop
  (pop
   (push
    (push
     (push (empty-stack)
          1)
     'c)
    'a))))
