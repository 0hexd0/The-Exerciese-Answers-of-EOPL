#lang racket
(require eopl)

(define identifier?
  (lambda (id)
    (if (eqv? id 'lambda)
        #f
        (symbol? id))))


(define-datatype proc proc?
  (procedure
   (var identifier?)
   (body expression?)
   (saved-env environments?)))

(define-datatype expval expval?
  (num-val
   (num number?))
  (proc-val 
   (proc proc?)))

(define report-expval-extractor-error
  (lambda (type val)
    (eopl:error "" val 'is 'not 'type 'of type)))

(define expval->num
  (lambda (val)
    (cases expval val
      (num-val (num) num)
      (else (report-expval-extractor-error 'num val)))))

(define expval->proc
  (lambda (val)
    (cases expval val
      (proc-val (proc) proc)
      (else (report-expval-extractor-error 'proc val)))))

(define-datatype environments environments?
  (empty-env)
  (extend-env
   (var identifier?)
   (val expval?)
   (old-env environments?)))

(define extend-env-by-lst
  (lambda (lst new-env old-env)
    (if (or
         (null? lst)
         (= 1
          (length lst)))
        new-env
        (extend-env-by-lst
         (cddr lst)
         (extend-env
          (car lst)
          (value-of
           (parse-single (cadr lst))
           old-env)
          new-env)
         old-env))))

(define extend-env-by-lst*
  (lambda (lst old-env)
    (if (or
         (null? lst)
         (= 1
          (length lst)))
        old-env
        (extend-env-by-lst*
         (cddr lst)
         (extend-env
          (car lst)
          (value-of
           (parse-single (cadr lst))
           old-env)
          old-env)))))

(define extend-env-by-two-lst
  (lambda (var-lst val-lst old-env)
    (if (null? var-lst)
        old-env
        (extend-env-by-two-lst
         (cdr var-lst)
         (cdr val-lst)
         (extend-env
          (car var-lst)
          (car val-lst)
          old-env)))))

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

(define-datatype bool-expression bool-expression?
  (bool-exp
   (num number?)))

(define value-of-bool-exp
  (lambda (exp)
    (cases bool-expression exp
      (bool-exp
       (num)
       (not (= num 0))))))

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
   (var-val-lst list?)
   (body expression?))
  (let*-exp
   (var-val-lst list?)
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
   (exp1 list?))
  (unpack-exp
   (var-lst identifier-list?)
   (val-lst expression?)
   (body expression?))
  (proc-exp
   (var identifier?)
   (body expression?))
  (call-exp
   (rator expression?)
   (rand expression?)))

(define identifier-list?
  (lambda (exp)
    (if (null? exp)
        #t
        (and (identifier? (car exp))
             (identifier-list? (cdr exp))))))

(define value-of
  (lambda (exp env)
    (cases expression exp
      (emptylist-exp () '())
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
                         (bool-exp 1)
                         (bool-exp 0)))))
      (equal?-exp (exp1 exp2)
                  (let ([val1 (value-of exp1 env)]
                        [val2 (value-of exp2 env)])
                  (let ([num1 (expval->num val1)]
                        [num2 (expval->num val2)])
                  (if (= num1 num2)
                      (bool-exp 1)
                      (bool-exp 0)))))
      (greater?-exp (exp1 exp2)
                    (let ([val1 (value-of exp1 env)]
                          [val2 (value-of exp2 env)])
                      (let ([num1 (expval->num val1)]
                            [num2 (expval->num val2)])
                        (if (> num1 num2)
                            (bool-exp 1)
                            (bool-exp 0)))))
      (less?-exp (exp1 exp2)
                 (let ([val1 (value-of exp1 env)]
                       [val2 (value-of exp2 env)])
                   (let ([num1 (expval->num val1)]
                         [num2 (expval->num val2)])
                     (if (< num1 num2)
                         (bool-exp 1)
                         (bool-exp 0)))))
      (if-exp (exp1 exp2 exp3)
              (let ([val1 (value-of-bool-exp exp1 env)])
                (if val1
                    (value-of exp2 env)
                    (value-of exp3 env))))
      (let-exp (var-val-lst body)
               (value-of body
                           (extend-env-by-lst var-val-lst env env)))
      (let*-exp (var-val-lst body)
                (value-of body
                          (extend-env-by-lst* var-val-lst env)))
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
                      [lst (value-of exp2 env)])
                  (cons val1 lst)))
      (list-exp (exp1)
                (map
                 (lambda (exp)
                   (value-of exp env))
                 exp1))
      (cond-exp (exp1)
                  (calc-cond-exp
                   (map
                    (lambda (exp)
                      (value-of exp env))
                    exp1)))
      (unpack-exp (var-lst val-lst body)
                  (value-of body
                            (extend-env-by-two-lst
                             var-lst
                             (value-of val-lst env)
                             env)))
      (proc-exp (var body)
                (proc-val
                 (procedure var body env)))
      
      (call-exp (rator rand)
                (let ([proc (expval->proc (value-of rator env))]
                      [arg (value-of rand env)])
                  (apply-procedure proc arg))))))

(define apply-procedure
  (lambda (proc1 val)
    (cases proc proc1
      (procedure (var body saved-env)
                 (value-of body (extend-env var val saved-env))))))

(define calc-cond-exp
  (lambda (lst)
    (if (null? lst)
        (eopl:error "lack condition for cond express" lst)
        (if (value-of-bool-exp
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
     (eqv? exp 'let*)
     (eqv? exp 'emptylist)
     (eqv? exp 'cons)
     (eqv? exp 'list)
     (eqv? exp 'cond)
     (eqv? exp 'unpack)
     (eqv? exp 'call)
     (eqv? exp 'proc))))

(define is-not-rator?
  (lambda (exp)
    (not
     (is-rator? exp))))

(define none-rator?
  (lambda (lst n)
    (cond ((= 0 n) #t)
          ((is-rator? (car lst)) #f)
          (else
           (none-rator? (cdr lst)
                        (- n 1))))))

(define is-let-lst?
  (lambda (lst)
    (cond
      ((null? lst)
       #f)
      ((= (length lst) 1)
       (is-not-rator? (car lst)))
      ((= (length lst) 2)
       #f)
      (else
       (if (and
            (identifier? (car lst))
            (is-not-rator? (car lst))
            (is-not-rator? (cadr lst)))
           (is-let-lst? (cddr lst))
           #f)))))

(define parse-let-lst
  (lambda (lst)
    (if
     (or
      (null? lst)
      (= 1
         (length lst)))
     lst
     (cons
      (car lst)
      (cons
       (parse-single (cadr lst))
       (parse-let-lst (cddr lst)))))))

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
              (is-let-lst? (cdr exp))
              (list
                 (let-exp (parse-let-lst
                           (take (cdr exp)
                                 (- (length (cdr exp)) 1)))
                          (parse-single (last exp))))
              (cons cur (replace (cdr exp)))))
            ((eqv? cur 'let*)
             (if
              (is-let-lst? (cdr exp))
              (list
                 (let*-exp (parse-let-lst
                            (take (cdr exp)
                                  (- (length (cdr exp)) 1)))
                           (parse-single (last exp))))
              (cons cur (replace (cdr exp)))))
            ((eqv? cur 'unpack)
             (if (none-rator? (cdr exp)
                              (length (cdr exp)))
                 (let ([var-lst (take (cdr exp)
                                      (- (length (cdr exp))
                                         2))]
                       [val-lst (parse-single (car (take-right exp 2)))]
                       [body (parse-single (last exp))])
                   (if
                    (and (identifier-list? var-lst)
                         (expression? val-lst)
                         (expression? body))
                    (list
                     (unpack-exp var-lst val-lst body))
                    (cons cur (replace (cdr exp)))))
                 (cons cur (replace (cdr exp)))))
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
            ((eqv? cur 'call)
             ((replace-factory 2 call-exp) exp))
            ((eqv? cur 'proc)
             (if
              (and (identifier? (cadr exp))
                   (is-not-rator? (caddr exp)))
              (cons
                 (proc-exp (cadr exp) (parse-single (caddr exp)))
                 (replace (cdddr exp)))
              (cons cur (cons (cadr exp) (replace (cddr exp))))))
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

(run '(let x 200 let f proc z - z x  let x 100 let g  proc z - z x  - call f 1 call g 1))