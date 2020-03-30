#lang racket
;Lc-exp ::= Identifier
;       ::= (lambda (Identifier) Lc-exp)
;       ::= (Lc-exp Lc-exp)

;var-exp : Var -> Lc-exp
(define var-exp
  (lambda (var)
    var))

;var-exp? : Lc-exp -> Bool
(define var-exp?
  (lambda (var)
    (symbol? var)))

;app-exp : Lc-exp * Lc-exp -> Lc-exp
(define app-exp
  (lambda (rator rand)
    (list rator rand)))

;app-exp? : Lc-exp -> Bool
(define app-exp?
  (lambda (exp)
    (= 2
       (length exp))))

;lambda-exp : Var * Lc-exp
(define lambda-exp
  (lambda (var body)
    (list 'lambda var body)))

;lambda-exp? : Lc-exp -> Bool
(define lambda-exp?
  (lambda (exp)
    (= 3
       (length exp))))

;var-exp->var : Lc-exp → Var
(define var-exp->var
  (lambda (exp)
    exp))

;lambda-exp->bound-var : Lc-exp → Var
(define lambda-exp->bound-var
  (lambda (exp)
    (cadr exp)))

;lambda-exp->body : Lc-exp → Lc-exp
(define lambda-exp->body
  (lambda (exp)
    (caddr exp)))

;app-exp->rator : Lc-exp → Lc-exp
(define app-exp->rator
  (lambda (exp)
    (car exp)))

;app-exp->rand : Lc-exp → Lc-exp
(define app-exp->rand
  (lambda (exp)
    (cadr exp)))

;occurs-free? : Sym × LcExp → Bool
(define occurs-free?
  (lambda (search-var exp)
    (cond
      ((var-exp? exp) (eqv? search-var (var-exp->var exp)))
      ((lambda-exp? exp)
       (and
        (not (eqv? search-var (lambda-exp->bound-var exp)))
        (occurs-free? search-var (lambda-exp->body exp))))
      (else
       (or
        (occurs-free? search-var (app-exp->rator exp))
        (occurs-free? search-var (app-exp->rand exp)))))))

(occurs-free? 'do '(lambda a (do a)))
(occurs-free? 'a '(lambda a (do a)))
