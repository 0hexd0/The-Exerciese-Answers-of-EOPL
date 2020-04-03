#lang racket
(require eopl)

(define identifier?
  (lambda (id)
    (if (eqv? id 'lambda)
        #f
        (symbol? id))))

(define-datatype expval expval?
  (num-val
   (num number?))
  (bool-val
   (bool boolean?))
  (list-val
   (list list?)))

(define report-expval-extractor-error
  (lambda (type val)
    (eopl:error "" val 'is 'not 'type 'of type)))

(define expval->num
  (lambda (val)
    (cases expval val
      (num-val (num) num)
      (else (report-expval-extractor-error 'num val)))))

(define expval->bool
  (lambda (val)
    (cases expval val
      (bool-val (bool) bool)
      (else (report-expval-extractor-error 'bool val)))))

(define expval->list
  (lambda (val)
    (cases expval val
      (list-val (list) list)
      (else (report-expval-extractor-error 'list val)))))

(define-datatype environments environments?
  (empty-env)
  (extend-env
   (var identifier?)
   (val expval?)
   (old-env environments?)))

(define apply-env
  (lambda (env exp)
    (cases environments env
      (empty-env ()
                 (eopl:error "empty-env bind no value" exp))
      (extend-env (var val old-env)
                  (if (eqv? exp var)
                      val
                      (apply-env old-env exp))))))

(define-datatype program program?
  (a-program
   (exp1 expression?)))

(define-datatype expression expression?
  (emptylist-exp)
  (const-exp
   (num number?))
  (list-exp
   (exp1 list?))
  (diff-exp
   (exp1 expression?)
   (exp2 expression?))
  (zero?-exp
   (exp1 expression?))
  (equal?-exp
   (exp1 expression?)
   (exp2 expression?))
  (greater?-exp
   (exp1 expression?)
   (exp2 expression?))
  (less?-exp
   (exp1 expression?)
   (exp2 expression?))
  (if-exp
   (exp1 expression?)
   (exp2 expression?)
   (exp3 expression?))
  (var-exp
   (var identifier?))
  (let-exp
   (var identifier?)
   (exp1 expression?)
   (body expression?))
  (minus-exp
   (const expression?))
  (addition-exp
   (exp1 expression?)
   (exp2 expression?))
  (multi-exp
   (exp1 expression?)
   (exp2 expression?))
  (quotient-exp
   (exp1 expression?)
   (exp2 expression?))
  (cons-exp
   (exp1 expression?)
   (exp2 expression?))
  (cond-exp
   (exp1 list?)))

(define value-of
  (lambda (exp env)
    (cases expression exp
      (emptylist-exp () (list-val '()))
      (const-exp (num) (num-val num))
      (var-exp (var) (apply-env env var))
      (diff-exp (exp1 exp2)
                (let ([val1 (value-of exp1 env)]
                      [val2 (value-of exp2 env)])
                  (let ([num1 (expval->num val1)]
                        [num2 (expval->num val2)])
                    (num-val
                     (- num1 num2)))))
      (zero?-exp (exp1)
                 (let ([val1 (value-of exp1 env)])
                   (let ([num1 (expval->num val1)])
                     (if (zero? num1)
                         (bool-val #t)
                         (bool-val #f)))))
      (equal?-exp (exp1 exp2)
                  (let ([val1 (value-of exp1 env)]
                        [val2 (value-of exp2 env)])
                  (let ([num1 (expval->num val1)]
                        [num2 (expval->num val2)])
                  (if (= num1 num2)
                      (bool-val #t)
                      (bool-val #f)))))
      (greater?-exp (exp1 exp2)
                    (let ([val1 (value-of exp1 env)]
                          [val2 (value-of exp2 env)])
                      (let ([num1 (expval->num val1)]
                            [num2 (expval->num val2)])
                        (if (> num1 num2)
                            (bool-val #t)
                            (bool-val #f)))))
      (less?-exp (exp1 exp2)
                 (let ([val1 (value-of exp1 env)]
                       [val2 (value-of exp2 env)])
                   (let ([num1 (expval->num val1)]
                         [num2 (expval->num val2)])
                     (if (< num1 num2)
                         (bool-val #t)
                         (bool-val #f)))))
      (if-exp (exp1 exp2 exp3)
              (let ([val1 (value-of exp1 env)])
                (if (expval->bool val1)
                    (value-of exp2 env)
                    (value-of exp3 env))))
      (let-exp (var exp1 body)
               (let ([val1 (value-of exp1 env)])
                 (value-of body
                           (extend-env var val1 env))))
      (minus-exp (exp1)
                 (num-val
                  (- (expval->num (value-of exp1 env)))))
      (addition-exp (exp1 exp2)
                (let ([val1 (value-of exp1 env)]
                      [val2 (value-of exp2 env)])
                  (let ([num1 (expval->num val1)]
                        [num2 (expval->num val2)])
                    (num-val
                     (+ num1 num2)))))
      (multi-exp (exp1 exp2)
                (let ([val1 (value-of exp1 env)]
                      [val2 (value-of exp2 env)])
                  (let ([num1 (expval->num val1)]
                        [num2 (expval->num val2)])
                    (num-val
                     (* num1 num2)))))
      (quotient-exp (exp1 exp2)
                (let ([val1 (value-of exp1 env)]
                      [val2 (value-of exp2 env)])
                  (let ([num1 (expval->num val1)]
                        [num2 (expval->num val2)])
                    (num-val
                     (quotient num1 num2)))))
      (cons-exp (exp1 exp2)
                (let ([val1 (value-of exp1 env)]
                      [val2 (value-of exp2 env)])
                  (let ([lst (expval->list val2)])
                    (list-val
                     (cons val1 lst)))))
      (list-exp (exp1)
                  (list-val
                   (map
                    (lambda (exp)
                      (value-of exp env))
                    exp1)))
      (cond-exp (exp1)
                  (calc-cond-exp
                   (map
                    (lambda (exp)
                      (value-of exp env))
                    exp1))))))

(define calc-cond-exp
  (lambda (lst)
    (if (null? lst)
        (eopl:error "lack condition for cond express" lst)
        (if (expval->bool
             (car lst))
            (cadr lst)
            (calc-cond-exp (cddr lst))))))

(define parse-single
  (lambda (exp)
    (cond
      ((integer? exp) (const-exp exp))
      ((identifier? exp) (var-exp exp))
      ((pair? exp) (parser exp))
      (else exp))))

(define is-rator?
  (lambda (exp)
    (or
     (eqv? exp '-)
     (eqv? exp '*)
     (eqv? exp '+)
     (eqv? exp '/)
     (eqv? exp 'minus)
     (eqv? exp 'if)
     (eqv? exp 'zero?)
     (eqv? exp '=)
     (eqv? exp '>)
     (eqv? exp '<)
     (eqv? exp 'let)
     (eqv? exp 'emptylist)
     (eqv? exp 'cons)
     (eqv? exp 'list)
     (eqv? exp 'cond))))

(define none-rator?
  (lambda (lst n)
    (cond ((= 0 n) #t)
          ((is-rator? (car lst)) #f)
          (else
           (none-rator? (cdr lst)
                        (- n 1))))))

(define sublist-until
  (lambda (lst item)
    (if (null? lst)
        '()
        (if (eqv? (car lst) item)
            '()
            (cons (car lst) (sublist-until (cdr lst) item))))))

(define replace-factory
  (lambda (oprand-num constructor)
    (lambda (exp)
      (if
       (= -1 oprand-num)
       (if (none-rator?
            (cdr exp)
            (length (cdr exp)))
           (list (constructor (map parse-single (cdr exp))))
           (cons (car exp) (replace (cdr exp))))
       (if (> (length exp) oprand-num)
           (if (none-rator? (cdr exp) oprand-num)
              (cond
                ((= oprand-num 0)
                 (cons
                  (constructor)
                  (replace (cdr exp))))
                ((= oprand-num 1)
                 (cons
                  (constructor
                   (parse-single
                    (cadr exp)))
                  (replace (cddr exp))))
                ((= oprand-num 2)
                 (cons
                  (constructor
                   (parse-single(cadr exp))
                   (parse-single(caddr exp)))
                  (replace (cdddr exp))))
                ((= oprand-num 3)
                 (cons
                  (constructor
                   (parse-single(cadr exp))
                   (parse-single(caddr exp))
                   (parse-single(cadddr exp)))
                  (replace (cddddr exp))))
                (else (eopl:error "too many oprands" exp)))
              (cons (car exp) (replace (cdr exp))))
           (eopl:error "illegal operation" exp))))))

(define replace
  (lambda (exp)
    (if (pair? exp)
        (let ([cur (car exp)])
          (cond
            ((eqv? cur 'let)
             (if
              (> (length exp) 3)
              (let ([next (cadr exp)]
                    [nnext (caddr exp)]
                    [nnnext (cadddr exp)])
                 (if
                  (and
                   (identifier? next)
                   (not (is-rator? nnext))
                   (not (is-rator? nnnext)))
                  (cons
                   (let-exp next (parse-single nnext) (parse-single nnnext))
                   (replace (cddddr exp)))
                  (cons cur (replace (cdr exp)))))
              (eopl:error "illegal operation" exp)))
            ((eqv? cur 'let)
             (if
              (> (length exp) 3)
              (let ([next (cadr exp)]
                    [nnext (caddr exp)]
                    [nnnext (cadddr exp)])
                 (if
                  (and
                   (identifier? next)
                   (not (is-rator? nnext))
                   (not (is-rator? nnnext)))
                  (cons
                   (let-exp next (parse-single nnext) (parse-single nnnext))
                   (replace (cddddr exp)))
                  (cons cur (replace (cdr exp)))))
              (eopl:error "illegal operation" exp)))
            ((eqv? cur '=)
             ((replace-factory 2 equal?-exp) exp))
            ((eqv? cur '>)
             ((replace-factory 2 greater?-exp) exp))
            ((eqv? cur '<)
             ((replace-factory 2 less?-exp) exp))
            ((eqv? cur '-)
             ((replace-factory 2 diff-exp) exp))
            ((eqv? cur 'if)
             ((replace-factory 3 if-exp) exp))
            ((eqv? cur '+)
             ((replace-factory 2 addition-exp) exp))
            ((eqv? cur 'minus)
             ((replace-factory 1 minus-exp) exp))
            ((eqv? cur 'zero?)
             ((replace-factory 1 zero?-exp) exp))
            ((eqv? cur '*)
             ((replace-factory 2 multi-exp) exp))
            ((eqv? cur '/)
             ((replace-factory 2 quotient-exp) exp))
            ((eqv? cur 'emptylist)
             ((replace-factory 0 emptylist-exp) exp))
            ((eqv? cur 'cons)
             ((replace-factory 2 cons-exp) exp))
            ((eqv? cur 'list)
             ((replace-factory -1 list-exp) exp))
            ((eqv? cur 'cond)
             (replace-with-end exp 'end cond-exp))
            (else
             (cons cur (replace (cdr exp))))))
        exp)))

(define replace-with-end
  (lambda (exp end-symbol constructor)
    (if (null? (cdr exp))
        (eopl:error "lack end-symbol for expression" exp)
        (let ([sublist (sublist-until (cdr exp) end-symbol)])
          (if (none-rator? sublist (length sublist))
              (cons (constructor
                     (map parse-single sublist))
                    (replace (cdr exp)))
              (cons (car exp) (replace (cdr exp))))))))

(define parser-inner
  (lambda (exp)
    (if (is-rator? (car exp))
        (parser-inner (replace exp))
        exp)))

(define parser
  (lambda (exp)
    (car (parser-inner exp))))

(define value-of-program
  (lambda (pgm)
    (cases program pgm
      (a-program (exp1)
                 (value-of exp1 (init-env))))))

(define run
  (lambda (lst)
    (value-of-program
     (a-program
      (parser lst)))))

(define init-env
  (lambda ()
    (extend-env
     'i (num-val 1)
     (extend-env
      'v (num-val 5)
      (extend-env
       'x (num-val 10)
       (empty-env))))))

(run '(- v cond = i 2 1 = i 1 2 end))