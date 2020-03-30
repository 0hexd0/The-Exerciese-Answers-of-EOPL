#lang racket
(require eopl)

(define-datatype bintree bintree?
  (leaf-node
   (num integer?))
  (interior-node
   (key symbol?)
   (left bintree?)
   (right bintree?)))

(define leaf-sum
  (lambda (exp)
    (cases bintree exp
      (leaf-node (num) num)
      (interior-node (key left right)
                     (+ (leaf-sum left)
                        (leaf-sum right))))))

(define get-key
  (lambda (exp)
    (cases bintree exp
      (leaf-node (num) num)
      (interior-node (key left right) key))))

(define max-interior
  (lambda (exp)
    (cases bintree exp
      (leaf-node (num) num)
      (interior-node (key left right)
                     (let ([left-sum (leaf-sum left)]
                           [right-sum (leaf-sum right)])
                       (cond
                         ((and (> left-sum 0)
                              (> right-sum 0))
                         key)
                         ((> right-sum left-sum) (get-key right))
                         (else (get-key left))))))))

(define tree-1
  (interior-node 'foo (leaf-node 2) (leaf-node 3)))
(define tree-2
  (interior-node 'bar (leaf-node -1) tree-1))
(define tree-3
  (interior-node 'baz tree-2 (leaf-node 1)))

(max-interior tree-2)
(max-interior tree-3)
