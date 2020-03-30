#lang racket
(require eopl)

(define list-of-red-blue-tree?
  (lambda (lst)
    (if (null? lst)
        #t
        (if (red-blue-tree? (car lst))
            (list-of-red-blue-tree? (cdr lst))
            #f))))

(define-datatype red-blue-tree red-blue-tree?
  (red-node
   (left red-blue-tree?)
   (right red-blue-tree?))
  (blue-node
   (children list-of-red-blue-tree?))
  (leaf-node
   (val integer?)))

(define converter-inner
  (lambda (exp number)
    (cases red-blue-tree exp
      (red-node (left right)
                (red-node (converter-inner left
                                           (+ number 1))
                          (converter-inner right
                                           (+ number 1))))
      (blue-node (children)
                 (blue-node
                  (map (lambda (tree)
                         (converter-inner tree
                                          number))
                       children)))
      (leaf-node (val)
                 (leaf-node number)))))

(define converter
  (lambda (exp)
    (converter-inner exp 0)))

(converter
 (blue-node
  (list
   (red-node
    (red-node
     (leaf-node 1)
     (leaf-node 2))
    (leaf-node 3))
   (leaf-node 4))))
