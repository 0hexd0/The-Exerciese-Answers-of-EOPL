#lang racket
(require eopl)

(define-datatype environments environments?
  (empty-env)
  (extend-env
   (old-env environments?)
   (binded-pair pair?)))

;has-binding? : Env * Var -> Boolean
(define has-binding?
  (lambda (exp search-var)
    (cases environments exp
      (empty-env () #f)
      (extend-env (old-env binded-pair)
                         (let ([saved-var (car binded-pair)])
                           (or (eqv? search-var saved-var)
                               (has-binding? old-env search-var)))))))

(define test-env
  (extend-env
   (extend-env (empty-env)
               '(a 1))
   '(b 2 )))

(has-binding? test-env 'c)
(has-binding? test-env 'a)
