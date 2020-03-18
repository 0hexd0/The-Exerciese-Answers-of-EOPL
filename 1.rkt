#lang racket
;1.15
;duple : Int * SchemeValue -> Listof(schemeValue)
;usage : returns a list containing n copies of x.
(define duple
  (lambda (n x)
    (cond ((= n 0) null)
        ((= n 1) (list x))
        (else
         (cons
          x
          (duple (- n 1) x))))))
;(duple 4 '(ha ha))

;1.16
;invert : Listof(2-lists) -> Listof(2-lists)
;returns a list with each 2-list reversed.
(define reverse
  (lambda (lst-of-two)
    (cons
     (cadr lst-of-two)
     (list (car lst-of-two)))))
(define invert
  (lambda (lst)
    (map reverse lst)))
;(invert '((a 1) (a 2) (1 b) (2 b)))

;1.17
;down: Listof(SchemeValue) -> Listof(SchemeValue)
;wraps parentheses around each top-level element of lst.
(define down-item
  (lambda (x)
    (list x)))
(define down
  (lambda (lst)
    (map down-item lst)))
;(down '(1 2 3))
;(down '((a) (fine) (idea)))
;(down '(a (more (complicated)) object))

;1.18
;swapper : Symbol * Symbol * Listof(Symbol) -> Listof(Sym)
;usage : (swapper s1 s2 slist) returns a list the same as slist, but with all occurrences of s1 replaced by s2 and all occurrences of s2 replaced by s1.
(define swapper-item
  (lambda (s1 s2 s)
    (if (symbol? s)
        (cond
          ((eqv? s1 s) s2)
          ((eqv? s2 s) s1)
          (else s))
        (swapper s1 s2 s))))
(define swapper
  (lambda (s1 s2 slist)
    (map
     (lambda (s)
       (swapper-item s1 s2 s))
     slist)
    ))
;(swapper 'a 'd '(a b c d))
;(swapper 'a 'd '(a d () c d))
;(swapper 'x 'y '((x) y (z (x))))

;1.19
;list-set : List * Int * SchemeValue -> List
;usage : (list-set lst n x) returns a list like lst, except that the n-th element, using zero-based indexing, is x.
(define list-set
  (lambda (lst n x)
    (if (= n 0)
        (cons x (cdr lst))
        (cons (car lst)
              (list-set
               (cdr lst)
               (- n 1)
               x)))))
;(list-set '(a b c d) 2 '(1 2))
;(list-ref (list-set '(a b c d) 3 '(1 5 10)) 3)

;1.20
;count-occurrences : Symbol * Listof(Symbol) -> Int
;usage (count-occurrences s slist) returns the number of occurrences of s in slist.
(define count-occurrences
  (lambda (s slist)
    (cond
      ((null? slist) 0)
      ((symbol? (car slist))
       (if (eqv? (car slist) s)
           (+ 1
              (count-occurrences s (cdr slist)))
           (count-occurrences s (cdr slist))))
      (else
       (+
        (count-occurrences s (car slist))
        (count-occurrences s (cdr slist)))))))
;(count-occurrences 'x '((f x) y (((x z) x))))
;(count-occurrences 'x '((f x) y (((x z) () x))))
;(count-occurrences 'w '((f x) y (((x z) x))))

;1.21
;product : Listof(Symbols) * Listof(Symbols) ->  Listof(2-lists)
;usage : (product sos1 sos2), where sos1 and sos2 are each a list of symbols without repetitions, returns a list of 2-lists that represents the Cartesian product of sos1 and sos2. The 2-lists may appear in any order.
(define product-item
  (lambda (sos symbol)
    (if (null? sos)
        '()
        (cons
         (list (car sos) symbol)
         (product-item (cdr sos) symbol)))))
(define concat
  (lambda (l1 l2)
    (if (null? l1)
        l2
        (cons
         (car l1)
         (concat (cdr l1) l2)))))
(define product
  (lambda (sos1 sos2)
    (if (null? sos2)
        '()
        (concat
         (product-item sos1 (car sos2))
         (product sos1 (cdr sos2))))))
;(product '(a b c) '(x y))

;1.22
;filter-in : Pred * List -> List
;(filter-in pred lst) returns the list of those elements in lst that satisfy the predicate pred.
(define filter-in
  (lambda (pred lst)
    (if (null? lst)
        '()
        (if (pred (car lst))
            (cons (car lst) (filter-in pred (cdr lst)))
            (filter-in pred (cdr lst))))))
;(filter-in number? '(a 2 (1 3) b 7))
;(filter-in symbol? '(a (b c) 17 foo))

;1.23
;list-index : Pred * List -> Scheme | #f
;usage : (list-index pred lst) returns the 0-based position of the first element of lst that satisfies the predicate pred. If no element of lst satisfies the predicate, then list-index returns #f.
(define list-index-inner
  (lambda (pred lst n)
    (if (null? lst)
        #f
        (if (pred (car lst))
            n
            (list-index-inner pred (cdr lst) (+ n 1))))))
(define list-index
  (lambda (pred lst)
    (list-index-inner pred lst 0)))
;(list-index number? '(a 2 (1 3) b 7))
;(list-index symbol? '(a (b c) 17 foo))
;(list-index symbol? '(1 2 (a b) 3))

;1.24
;every : Pred * List -> Boolean
;usage : (every? pred lst) returns #f if any element of lst fails to satisfy pred, and returns #t otherwise.
(define every?
  (lambda (pred lst)
    (if (null? lst)
        #t
        (if (pred (car lst))
            (every? pred (cdr lst))
            #f))))
;(every? number? '(a b c 3 e))
;(every? number? '(1 2 3 5 4))

;1.25
;exists? : Pred * List -> Boolean
;usage: (exists? pred lst) returns #t if any element of lst satisfies pred, and returns #f otherwise.
(define exists?
  (lambda (pred lst)
    (if (null? lst)
        #f
        (if (pred (car lst))
            #t
            (exists? pred (cdr lst))))))
;(exists? number? '(a b c 3 e))
;(exists? number? '(a b c d e))

;1.26
;up: List -> List
;usage : (up lst) removes a pair of parentheses from each top-level element of lst. If a top-level element is not a list, it is included in the result, as is.The value of (up (down lst)) is equivalent to lst, but (down (up lst)) is not necessarily lst.
(define up
  (lambda (lst)
    (if (null? lst)
        '()
        (if (symbol? (car lst))
            (concat (list (car lst)) (up (cdr lst)))
            (concat (car lst) (up (cdr lst)))))))
;(up '((1 2) (3 4)))
;(up '((x (y)) z))

;1.27
;flatten : Listof(Symbols) -> List
;usage : (flatten slist) returns a list of the symbols contained in slist in the order in which they occur when slist is printed. Intuitively, flatten removes all the inner parentheses from its argument.
(define flatten
  (lambda (slist)
    (if (null? slist)
        '()
        (if (symbol? (car slist))
            (concat (list (car slist)) (flatten (cdr slist)))
            (concat (flatten (car slist)) (flatten (cdr slist)))))))
;(flatten '(a b c))
;(flatten '((a) () (b ()) () (c)))
;(flatten '((a b) c (((d)) e)))
;(flatten '(a b (() (c))))

;1.28
;merge : Listof(Int) * Listof(Int) -> Listof(Int)
;usage : (merge loi1 loi2), where loi1 and loi2 are lists of integers that are sorted in ascending order, returns a sorted list of all the integers in loi1 and loi2.
(define merge
  (lambda (loi1 loi2)
    (cond
      ((null? loi1) loi2)
      ((null? loi2) loi1)
      ((< (car loi1) (car loi2))
       (cons
        (car loi1)
        (merge (cdr loi1) loi2)))
      (else
       (cons
        (car loi2)
        (merge loi1 (cdr loi2)))))))
;(merge '(1 4) '(1 2 8))
;(merge '(35 62 81 90 91) '(3 83 85 90))


;1.29
;sort: Listof(Int) -> Listof(Int)
;usage: (sort loi) returns a list of the elements of loi in ascending order.
(define insert-ascending
  (lambda (loi i)
    (if (null? loi)
        (list i)
        (if (< i (car loi))
            (cons i loi)
            (cons
             (car loi)
             (insert-ascending
              (cdr loi)
              i))))))
(define sort
  (lambda (loi)
    (if (null? loi)
        '()
       (insert-ascending
        (sort (cdr loi))
        (car loi)))))
;(sort '(8 2 5 2 3))

;1.30
;sort/predicate: Pred * Listof(Int) -> Listof(Int)
;usage: (sort/predicate pred loi) returns a list of elements sorted by the predicate.
(define insert-ascending/predicate
  (lambda (pred loi i)
    (if (null? loi)
        (list i)
        (if (pred i (car loi))
            (cons i loi)
            (cons
             (car loi)
             (insert-ascending/predicate
              pred
              (cdr loi)
              i))))))
(define sort/predicate
  (lambda (pred loi)
    (if (null? loi)
        '()
       (insert-ascending/predicate
        pred
        (sort/predicate pred (cdr loi))
        (car loi)))))
;(sort/predicate < '(8 2 5 2 3))
;(sort/predicate > '(8 2 5 2 3))

;1.31
;Bintree ::= Int | (Symbol Bintree Bintree)
;Write the following procedures for calculating on a bintree (definition 1.1.7): leaf and interior-node, which build bintrees, leaf?, which tests whether a bintree is a leaf, and lson, rson, and contents-of, which extract the components of a node. contents-of should work on both leaves and interior nodes.
(define leaf
  (lambda (i)
    i))
(define interior-node
  (lambda (s lson rson)
    (list s lson rson)))
(define leaf?
  (lambda (node)
    (number? node)))
(define lson
  (lambda (tree)
    (cadr tree)))
(define rson
  (lambda (tree)
    (caddr tree)))
(define contents-of
  (lambda (node)
    (if (leaf? node)
        node
        (car node))))

;1.32
;double-tree: Bintree -> Bintree 
;usage: Write a procedure double-tree that takes a bintree, as represented in definition 1.1.7, and produces another bintree like the original, but with all the integers in the leaves doubled.
(define double-int
  (lambda (i)
    (* i 2)))
(define double-tree
  (lambda (tree)
   (if (leaf? tree)
        (leaf
         (double-int
          (contents-of tree)))
        (interior-node (contents-of tree)
                       (double-tree (lson tree))
                       (double-tree (rson tree))))))
(define test-tree (interior-node 'red
               (interior-node 'bar
                              (leaf 26)
                              (leaf 12))
               (interior-node 'red
                              (leaf 11)
                              (interior-node 'quux
                                             (leaf 117)
                                             (leaf 14)))))
;(double-tree test-tree)

;1.33
;mark-leaves-with-red-depth: Bintree -> Bintree
;usage: Write a procedure mark-leaves-with-red-depth that takes a bintree (definition 1.1.7), and produces a bintree of the same shape as the original, except that in the new tree, each leaf contains the integer of nodes between it and the root that contain the symbol red. For example, the expression
(define mark-leaves-with-red-depth-inner
  (lambda (tree pNum)
    (if (leaf? tree)
        pNum
        (interior-node
         (contents-of tree)
         (mark-leaves-with-red-depth-inner
          (lson tree)
          (if (eqv? (contents-of tree) 'red)
              (+ 1 pNum)
              pNum))
         (mark-leaves-with-red-depth-inner
          (rson tree)
          (if (eqv? (contents-of tree) 'red)
              (+ 1 pNum)
              pNum))))))
(define mark-leaves-with-red-depth
  (lambda (tree)
    (mark-leaves-with-red-depth-inner tree 0)))
;(mark-leaves-with-red-depth test-tree)

;1.34
;Binary-search-tree ::=()|(Int Binary-search-tree Binary-search-tree)
;path: BinaryTree * Int -> List(Left|Right)
;usage: Write a procedure path that takes an integer n and a binary search tree bst (page 10) that contains the integer n, and returns a list of lefts and rights showing how to find the node containing n. If n is found at the root, it returns the empty list.
(define s-leaf '())
(define s-interior-node
  (lambda (i lson rson)
    (list i lson rson)))
(define s-leaf?
  (lambda (node)
    (null? node)))
(define s-lson
  (lambda (tree)
    (cadr tree)))
(define s-rson
  (lambda (tree)
    (caddr tree)))
(define s-contents-of
  (lambda (node)
    (if (s-leaf? node)
        node
        (car node))))
(define s-test-tree
  '(14 (7 () (12 () ()))
       (26 (20 (17 () ())
               ())
           (31 () ()))))
(define path-inner
  (lambda (s-tree n list)
    (if (s-leaf? s-tree)
        list
        (cond
          ((< n (s-contents-of s-tree)) (path-inner (s-lson s-tree) n (concat list '(left))))
          ((> n (s-contents-of s-tree)) (path-inner (s-rson s-tree) n (concat list '(right))))
          (else list)))))
(define path
  (lambda (s-tree n)
    (path-inner s-tree n '())))
;(path s-test-tree 14)
;(path s-test-tree 17)

;1.35 Bintree -> Bintree
;number-leaves: 
;usage: Write a procedure number-leaves that takes a bintree, and produces a bintree like the original, except the contents of the leaves are numbered starting from 0. For example
(define counter-of-tree
  (lambda (tree)
    (if (leaf? tree)
        (+ 1 (contents-of tree))
        (counter-of-tree (rson tree)))))

(define number-leaves-inner
  (lambda (tree last-tree-n)
    (if (leaf? tree)
        last-tree-n
        (interior-node
         (contents-of tree)
         (number-leaves-inner
          (lson tree)
          last-tree-n)
         (number-leaves-inner
          (rson tree)
          (counter-of-tree
           (number-leaves-inner
            (lson tree)
            last-tree-n)))))))

(define number-leaves
  (lambda (tree)
    (number-leaves-inner tree 0)))

(define test-tree-new (interior-node 'foo
                                     (interior-node 'bar
                                                    (leaf 26)
                                                    (leaf 12))
                                     (interior-node 'baz
                                                    (leaf 11)
                                                    (interior-node 'quux
                                                                   (interior-node 'tomac
                                                                                  (leaf 1)
                                                                                  (interior-node 'faker
                                                                                                 (leaf 32)
                                                                                                 (leaf 4)))
                                                                   (leaf 14)))))
;(number-leaves test-tree-new)

;1.37
;g: FirstList * Recu -> List
;usage:  Write a procedure g such that number-elements from page 23 could be defined as
(define change-idx
  (lambda (lst)
    (change-idx-inner lst 0)))

(define change-idx-inner
  (lambda (lst n)
    (if (null? lst)
        '()
        (cons
         (cons n (cdar lst))
         (change-idx-inner (cdr lst) (+ n 1))))))

(define g
  (lambda (lst recursion)
    (change-idx
     (concat
      (list
       (cons
        (length recursion)
        (cdr lst)))
      recursion))))

(define number-elements
  (lambda (lst)
   (if (null? lst) '()
        (g (list 0 (car lst)) (number-elements (cdr lst))))))

;(number-elements '(a b c d e f))
