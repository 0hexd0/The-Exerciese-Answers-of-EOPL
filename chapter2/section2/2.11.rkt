#lang racket
(require eopl)

;empty-env : () -> Env
(define empty-env
  (lambda () '()))

;extend-env : Var * SchemeVal * Env -> Env
(define extend-env
  (lambda (var val env)
    (list env
          (list (list var) (list val)))))

(define report-no-binding-found
  (lambda (search-var)
    (eopl:error 'apply-env "No binding for ~s" search-var)))

(define find-inner
  (lambda (lst item n)
    (if (null? lst)
        -1
        (if (eqv? (car lst)
               item)
            n
            (find-inner (cdr lst)
                        item
                        (+ n
                           1))))))

(define find
  (lambda (lst item)
    (find-inner lst item 0)))

(define get
  (lambda (lst idx)
    (if (= 0 idx)
        (car lst)
        (get (cdr lst) (- idx 1)))))

;apply-env : Env * Var -> SchemeVal
(define apply-env
  (lambda (env search-var)
    (if (null? env)
        (report-no-binding-found search-var)
        (let ((saved-vars (caadr env))
              (saved-vals (cadadr env))
              (saved-env (car env))
              (findedIdx (find (caadr env) search-var)))
          (if (> findedIdx -1)
                 (get saved-vals findedIdx)
                 (apply-env saved-env search-var))))))

;empty-env? : Env -> Boolean
(define empty-env?
  (lambda (env)
    (null? env)))

;has-binding? : Env * Var -> Boolean
(define has-binding?
  (lambda (env search-var)
    (if (null? env)
        #f
        (let ((saved-vars (caadr env))
              (saved-vals (cadadr env))
              (saved-env (car env))
              (findedIdx (find (caadr env) search-var)))
          (if (> findedIdx -1)
                 #t
                 (has-binding? saved-env search-var))))))

;extend-env* : Env * Listof(Var and SchemeVal) -> Env
(define extend-env*
  (lambda (vars vals env)
    (list env
          (list vars vals))))

(define test-env
  (extend-env* '(a b e) '(1 2 5) (empty-env)))

test-env
(apply-env test-env 'e)
