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
           (lambda-exp
            (car (cadr datum))
            (parse-expression (caddr datum)))
           (app-exp
            (parse-expression (car datum))
            (parse-expression (cadr datum)))))
      (else (eopl:error "invalid-concrete-syntax")))))

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

(define unparser
  (lambda (exp)
    (cases lc-exp exp
      (var-exp (var) (list 'var-exp (list var)))
      (lambda-exp (bound-var body)
                  (list 'lambda-exp (list 'var-exp (list bound-var))
                        (unparser body)))
      (app-exp (rator rand)
               (list 'app-exp
                     (list (unparser rator)
                           (unparser rand)))))))

(unparser (parse-expression '(lambda (a) (do b))))