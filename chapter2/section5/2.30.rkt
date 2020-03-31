#lang racket
(require eopl)

(define identifier?
  (lambda (id)
    (if (eqv? id 'lambda)
        #f
        (symbol? id))))

(define-datatype lc-exp lc-exp?
  (var-exp
   (var identifier?))
  (lambda-exp
   (bound-var identifier?)
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
           (if (= 2
                  (length (cdr datum)))
               (if (and
                    (pair? (cadr datum))
                    (= 1 (length
                          (cadr datum))))
                   (lambda-exp
                    (car (cadr datum))
                    (parse-expression (caddr datum)))
                   (eopl:error "invalid lambda-exp bound-var" (cadr datum)))
               (eopl:error "invalid lambda-exp" datum))
           (if (= (length datum) 2)
               (app-exp
                (parse-expression (car datum))
                (parse-expression (cadr datum)))
               (eopl:error "invalid app-exp" datum))))
      (else (eopl:error "invalid concrete-syntax" datum)))))

(define unparse-lc-exp
  (lambda (exp)
    (cases lc-exp exp
      (var-exp (var) var)
      (lambda-exp (bound-var body)
                  (list 'lambda (list bound-var)
                        (unparse-lc-exp body)))
      (app-exp (rator rand)
               (list (unparse-lc-exp rator)
                     (unparse-lc-exp rand))))))

;(parse-expression '(lambda (a b v) (a b)))
;(parse-expression '(lambda))
(parse-expression '(lambda a b))