#lang racket
(require eopl)

(define list-of
  (lambda (pred)
    (lambda (val)
      (if (null? val)
        #t
         (and (list? val)
              (pred (car val))
              ((list-of pred) (cdr val)))))))

(define identifier?
  (lambda (id)
    (if (eqv? id 'lambda)
        #f
        (symbol? id))))

(define-datatype lc-exp lc-exp?
  (var-exp
   (var identifier?))
  (lambda-exp
   (bound-vars (list-of identifier?))
   (body lc-exp?))
  (app-exp
   (rator lc-exp?)
   (rand lc-exp?)))

(define parse-expression
  (lambda (datum)
    (cond
      ((symbol? datum) (var-exp datum))
      ((pair? datum)
       (if (eqv? (car datum) 'lambda)
           (lambda-exp
            (cadr datum)
            (parse-expression (caddr datum)))
           (app-exp
            (parse-expression (car datum))
            (parse-expression (cadr datum)))))
      (else (eopl:error "invalid-concrete-syntax")))))

(define unparse-lc-exp
  (lambda (exp)
    (cases lc-exp exp
      (var-exp (var) var)
      (lambda-exp (bound-vars body)
                  (list 'lambda bound-vars
                        (unparse-lc-exp body)))
      (app-exp (rator rand)
               (list (unparse-lc-exp rator)
                     (unparse-lc-exp rand))))))

(define test-exp
  '(lambda (a b) (do b)))

(parse-expression test-exp)

(unparse-lc-exp
 (parse-expression test-exp))