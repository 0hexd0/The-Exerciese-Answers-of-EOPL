#lang racket
(require eopl)

(define-datatype prefix-exp prefix-exp?
  (const-exp
   (num integer?))
  (diff-exp
   (operand1 prefix-exp?)
   (operand2 prefix-exp?)))

(define parse-single
  (lambda (exp)
    (if (integer? exp)
        (const-exp exp)
        exp)))

(define replace
  (lambda (exp)
    (if (and (pair? exp)
             (> (length exp) 2))
        (let ([cur (car exp)]
              [next (cadr exp)]
              [nnext (caddr exp)])
          (if (and
               (eqv? cur '-)
               (not (eqv? next '-))
               (not (eqv? nnext '-)))
              (cons
               (diff-exp (parse-single next) (parse-single nnext))
               (replace (cdddr exp)))
              (cons cur (replace (cdr exp)))))
        exp)))

(define parser-inner
  (lambda (exp)
    (if (eqv? (car exp) '-)
        (parser-inner (replace exp))
        exp)))

(define parser
  (lambda (exp)
    (car (parser-inner exp))))

(parser '(- - 3 2 - 4 - 12 7))
(parser '(- - - - 5 6 3 4 - 3 4 ))


