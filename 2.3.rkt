#lang racket

;Diff-tree::=(one) | (diff Diff-tree Diff-tree)

(define base '(one))

(define is-base?
  (lambda (tree)
    (eqv?
     tree
     base)))

(define diff
  (lambda (lson rson)
    (list 'diff lson rson)))

(define zero (diff base base))

(define two
  (diff
   base
   (diff zero base)))

(define lson
  (lambda (tree)
    (cadr tree)))

(define rson
  (lambda (tree)
    (caddr tree)))

(define calc
  (lambda (tree)
    (if (is-base? tree)
     1
     (-
      (calc (lson tree))
      (calc (rson tree))))))

(define is-zero?
  (lambda (tree)
    (=
     (calc tree)
     0)))

(define is-one?
  (lambda (tree)
    (=
     (calc tree)
     1)))

(define diff-tree-plus
 (lambda (t1 t2)
   (diff t1
         (diff
          zero
          t2))))

(define successor
  (lambda (tree)
    (if (is-one? tree)
        two
        (diff
         (successor (lson tree))
         (rson tree)))))

(define predecessor
  (lambda (tree)
    (if (is-one? tree)
        zero
        (diff
         (predecessor (lson tree))
         (rson tree)))))

(calc (predecessor (predecessor (successor (successor two)))))
(calc (predecessor (diff-tree-plus (diff-tree-plus base two) two)))
