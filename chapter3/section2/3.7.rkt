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
   (bool boolean?)))

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
  (const-exp
   (num number?))
  (diff-exp
   (exp1 expression?)
   (exp2 expression?))
  (zero?-exp
   (exp1 expression?))
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
   (exp2 expression?)))

(define value-of
  (lambda (exp env)
    (cases expression exp
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
                     (quotient num1 num2))))))))

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
     (eqv? exp 'zero?))))

(define replace
  (lambda (exp)
    (if (pair? exp)
        (let ([cur (car exp)])
          (cond
            ((eqv? cur '-)
             (if
              (> (length exp) 2)
              (let ([next (cadr exp)]
                    [nnext (caddr exp)])
                 (if
                  (and
                   (not (is-rator? next))
                   (not (is-rator? nnext)))
                  (cons
                   (diff-exp (parse-single next) (parse-single nnext))
                   (replace (cdddr exp)))
                  (cons cur (replace (cdr exp)))))
              (eopl:error "illegal operation" exp)))
            ((eqv? cur '+)
             (if
              (> (length exp) 2)
              (let ([next (cadr exp)]
                    [nnext (caddr exp)])
                 (if
                  (and
                   (not (is-rator? next))
                   (not (is-rator? nnext)))
                  (cons
                   (addition-exp (parse-single next) (parse-single nnext))
                   (replace (cdddr exp)))
                  (cons cur (replace (cdr exp)))))
              (eopl:error "illegal operation" exp)))
            ((eqv? cur 'minus)
             (if
              (> (length exp) 1)
              (let ([next (cadr exp)])
                (if
                  (not (is-rator? next))
                  (cons
                   (minus-exp (parse-single next))
                   (replace (cddr exp)))
                  (cons cur (replace (cdr exp)))))
              (eopl:error "illegal operation" exp)))
            ((eqv? cur 'zero?)
             (if
              (> (length exp) 1)
              (let ([next (cadr exp)])
                (if
                  (not (is-rator? next))
                  (cons
                   (zero?-exp (parse-single next))
                   (replace (cddr exp)))
                  (cons cur (replace (cdr exp)))))
              (eopl:error "illegal operation" exp)))
            ((eqv? cur '*)
             (if
              (> (length exp) 2)
              (let ([next (cadr exp)]
                    [nnext (caddr exp)])
                (if
                  (and
                   (not (is-rator? next))
                   (not (is-rator? nnext)))
                  (cons
                   (multi-exp (parse-single next) (parse-single nnext))
                   (replace (cdddr exp)))
                  (cons cur (replace (cdr exp)))))
              (eopl:error "illegal operation" exp)))
            ((eqv? cur '/)
             (if
              (> (length exp) 2)
              (let ([next (cadr exp)]
                    [nnext (caddr exp)])
                (if
                  (and
                   (not (is-rator? next))
                   (not (is-rator? nnext)))
                  (cons
                   (quotient-exp (parse-single next) (parse-single nnext))
                   (replace (cdddr exp)))
                  (cons cur (replace (cdr exp)))))
              (eopl:error "illegal operation" exp)))
            (else
             (cons cur (replace (cdr exp))))))
        exp)))

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

(run '(- (+ 3 2) - (/ 6 2) (* 2 1)))
(run '(- + 3 2 - (/ 6 2) (* 2 1)))
(run '(- + 3 2 - / 6 2 (* 2 1)))
(run '(- + 3 2 - / 6 2 * 2 1))
(run '(zero? (- 1 1)))
