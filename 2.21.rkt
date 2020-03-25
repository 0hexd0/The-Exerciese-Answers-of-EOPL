#lang racket
(require eopl)

(define empty-env
  (lambda () '()))

(define extend-env
  (lambda (var val env)
    (list env
          (list var val))))

(define-datatype environments environments?
  (empty-env-exp)
  (non-empty-env-exp
   (old-env environments?)
   (var-lst var-lst?)))

(define-datatype var-lst var-lst?
  (lst-exp
   (var symbol?)))

;has-binding? : Env * Var -> Boolean
(define has-binding?
  (lambda (exp search-var)
    (cases environments exp
      (empty-env-exp () #f)
      (non-empty-env-exp (old-env var-lst)
                         (let ([saved-var (car var-lst)])
                           (or (eqv? search-var saved-var)
                               (has-binding? old-env search-var)))))))

(define test-env
  (extend-env 'a 1 (empty-env)))

(has-binding? '() 'a)
