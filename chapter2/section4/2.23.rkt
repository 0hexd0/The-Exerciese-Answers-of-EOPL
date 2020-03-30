#lang racket
(require eopl)

;LcExp ::= Identifier
;      ::= (lambda (Identifier) LcExp)
;      ::= (LcExp LcExp)

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

(lc-exp? (var-exp 'a))
(lc-exp? (var-exp 'lambda))
