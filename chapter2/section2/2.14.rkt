#lang racket

(define empty-env
  (lambda ()
    (list
     (lambda (search-var)
       null)
     (lambda ()
       #t))))

(define extend-env
  (lambda (saved-var saved-val saved-env)
    (list
     (lambda (search-var)
       (if (eqv? search-var saved-var)
           saved-val
           (apply-env saved-env search-var)))
     (lambda ()
       #f))))

(define apply-env
  (lambda (env search-var)
    ((car env) search-var)))

(define empty-env?
  (lambda (env)
    ((cadr env))))

(define has-binding?
  (lambda (env search-var)
    (if (empty-env? env)
        #f
        (not (null? (apply-env env search-var))))))

(has-binding? (empty-env) 'a)
(has-binding? (extend-env 'a 1 (empty-env)) 'a)
(has-binding? (extend-env 'a 1 (empty-env)) 'b)
