#lang racket
(require eopl)

(define legal-stack-val?
  (lambda (test)
    (or
     (symbol? test)
     (number? test))))

(define-datatype stack stack?
  (empty-stack)
  (no-empty-stack
   (last-stack stack?)
   (val legal-stack-val?)))

(define push no-empty-stack)

(define pop
  (lambda (sth)
    (cases stack sth
      (empty-stack () (eopl:error "empty stack can not pop"))
      (no-empty-stack (last-stack val) last-stack))))

(define top
  (lambda (sth)
    (cases stack sth
      (empty-stack () (eopl:error "empty stack can not top"))
      (no-empty-stack (last-stack val) val))))

;(define pop (lambda () ()))
(top
 (push (empty-stack) '1))

(top
 (push
  (push (empty-stack) '1)
  2))

(top
 (pop
  (push
   (push (empty-stack) '1)
   2)))

(top
 (pop
  (push (empty-stack) '1)))
