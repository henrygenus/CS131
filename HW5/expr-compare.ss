#lang racket
(provide (all-defined-out))

; variables for testing purposes
;

(define test-expr-x '(+ 3 ((lambda (λ lambda) (if (> lambda λ) (- lambda λ) (+ lambda λ))) 2 1)))
(define test-expr-y '(- 3 ((λ (lambda λ) (if (> lambda λ) (- lambda λ) (+ λ lambda))) 1 2)))
(define % #t)
(define (test-test) (test-expr-compare test-expr-x test-expr-y))
(define (test) (expr-compare test-expr-x test-expr-y))

; function which calls expr-compare and evaluates results for %/{#t, #f)
; comparing the former to x and the latter to y
;
(define (test-expr-compare x y) ;currently not working since lambda is incomplete
  (let ([expr (expr-compare x y)])
    (if (and (let ([% #t]) (eqv? (eval expr) (eval x)))
             (let ([% #f]) (eqv? (eval expr) (eval y))))
        #t #f)))

; compares two Scheme expressions x & y and produces a difference summary of the two
;
(define (expr-compare x y)
  (cond [(not (and (list? x) (list? y))) (expr-unify x y)]
        [(or (eqv? (car x) 'quote) (eqv? (car y) 'quote)) (expr-compare-quote x y)]
        [(or (eqv? (car x) 'if) (eqv? (car y) 'if)) (expr-compare-if x y)]
        [(or (eqv? (car x) 'λ) (eqv? (car x) 'lambda) (eqv? (car y) 'λ) (eqv? (car y) 'lambda)) (expr-compare-lambda x y)]
        [#t (let map-compare ((x x) (y y))
              (cond [(and (null? x) (null? y)) '()]
                    [(not (eqv? (null? (cdr x)) (null? (cdr y)))) (expr-unify x y)]
                    [#t (cons (expr-compare (car x) (car y))
                              (map-compare (cdr x) (cdr y)))]))]))

; finds the most general binding for two expressions
;
(define (expr-unify x y)
  (if (or (boolean? x) (boolean? y))
      (atom-unify-bool x y)
      (atom-unify x y)))

; merge booleans before finding a single binding which matches both X and Y
; transforms: {#t & #t ->#t, #f & #f -> #f, #t & #f -> '%, #f & #t -> '(not %)}
; second level: {y/#t -> '(not %), y/#f -> '%, x/#t -> '%, x/#f -> '(not%)}
;
(define (atom-unify-bool x y)
  (cond [(and (eq? x #t) (eq? y #f)) '%]
        [(and (eq? x #f) (eq? y #t)) '(not %)]
        [(and (eq? x #t) (eq? y #t)) #t]
        [(and (eq? x #f) (eq? y #f)) #f]
        [#t (let ([x (cond [(eq? x #t) '%] [(eq? x #f) '(not %)] [#t x])]
                  [y (cond [(eq? y #t) '(not %)] [(eq? y #f) '%] [#t y])])
              (atom-unify x y))]))

; find a single binding which satisfies both atoms -- may be conditional
;
(define (atom-unify x y)
  (if (eqv? x y)
      x
      (list 'if '% x y)))

; compares expressions of the form (quote EXPR)
;
(define (expr-compare-quote x y)
  (if (not (eqv? (car x) (car y)))
      (list 'if '% x y)
      (atom-unify x y)))

; compares expressions of the form (if EXPR EXPR EXPR)
;
(define (expr-compare-if x y)
  (if (not (eqv? (car x) (car y)))
      (list 'if '%
            (cons (car x) (map expr-compare (cdr x) (cdr x)))
            (cons (car y) (map expr-compare (cdr y) (cdr y))))
      (list 'if (expr-compare (cadr x) (cadr y))
            (expr-compare (caddr x) (caddr y))
            (expr-compare (cadddr x) (cadddr y)))))


; compares expressions of the form (lambda FORMALS EXPR)
;
(define (expr-compare-lambda x y)
  (let ([x (cons (blend-lambdas (car x) (car y)) (cdr x))]
        [y (cons (blend-lambdas (car y) (car x)) (cdr y))])
    (if (or (not (eqv? (car x) (car y))) (not (= (length (cadr x)) (length (cadr y)))))
        (list 'if '%
              (list (car x) (cadr x) (expr-compare (caddr x) (caddr x)))
              (list (car y) (cadr y) (expr-compare (caddr y) (caddr y))))
        (cons (expr-unify (car x) (car y))
              (lambda-blend-params (cadr x) (cddr x) (cadr y) (cddr y))
              ))))

(define (blend-lambdas l1 l2)
  (if (or (and (eqv? l1 'lambda) (eqv? l2 'λ))
          (and (eqv? l1 'λ) (eqv? l2 'lambda)))
      'λ l1))

; compares parameters for x and y, blending if necessary
; invariant: len params x = len params y
;
(define (lambda-blend-params x-param x-expr y-param y-expr)
  (list (atom-unify x-param y-param) (expr-compare x-expr y-expr)))

               ; map parameters
               ; compare expressions
               ; ret '(params exprs)'
  