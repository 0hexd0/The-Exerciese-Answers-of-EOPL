#lang racket
(require eopl)

;empty-env : () -> Env
(define empty-env
  (lambda () '()))

;extend-env : Var * SchemeVal * Env -> Env
(define extend-env
  (lambda (var val env)
    (list env
          (list var val))))

(define report-no-binding-found
  (lambda (search-var)
    (eopl:error 'apply-env "No binding for ~s" search-var)))

;apply-env : Env * Var -> SchemeVal
(define apply-env
  (lambda (env search-var)
    (if (null? env)
        (report-no-binding-found search-var)
        (let ((saved-var (caadr env))
              (saved-val (cadadr env))
              (saved-env (car env)))
             (if (eqv? search-var saved-var)
                 saved-val
                 (apply-env saved-env search-var))))))

;empty-env? : Env -> Boolean
(define empty-env?
  (lambda (env)
    (null? env)))

(define test-env1 (empty-env))
(define test-env2 (extend-env 'a 1 (empty-env)))

(empty-env? test-env1)
(empty-env? test-env2)
