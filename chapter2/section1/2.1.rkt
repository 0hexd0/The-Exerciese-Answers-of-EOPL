#lang racket
;2.1
;BigInt
(define zero '(0))
(define one '(1))
;二进制
;(define N 2)
;(define ten '(0 1 0 1))
;(define five '(1 0 1 0))
;八进制
(define N 8)
(define ten '(2 1))
(define five '(5))
;十六进制
;(define N 16)
;(define ten '(10))
;(define five '(5))

(define is-zero?
  (lambda (n)
    (if (null? n)
        #t
        (if (eqv? 0 (car n))
            (is-zero? (cdr n))
            #f))))

(define is-one?
  (lambda (n)
    (if (null? n)
        #t
        (if (eqv? 1 (car n))
            (is-zero? (cdr n))
            #f))))

(define successor
  (lambda (n)
    (if
     (< (car n) (- N 1))
     (cons
      (+ 1 (car n))
      (cdr n))
     (if
      (= (length n) 1)
      '(0 1)
      (cons
       0
       (successor (cdr n)))))))

(define predecessor
  (lambda (n)
    (if
     (> (car n) 0)
     (cons
      (- (car n) 1)
      (cdr n))
     (cons
       (- N 1)
       (predecessor (cdr n)))
     )))

(define plus
  (lambda (x y)
    (if (is-zero? x)
        y
        (successor (plus (predecessor x) y)))))

(define subtract
  (lambda (x y)
    (if (is-zero? y)
        x
        (predecessor (subtract x (predecessor y))))))

(define multiply
  (lambda (x y)
    (if
     (is-zero? y)
     zero
     (plus
      x
      (multiply
       x
       (predecessor y))))))

(define factorial
  (lambda (n)
    (if (is-one? n)
        n
        (multiply
         n
         (factorial (predecessor n))))))

(factorial ten)
