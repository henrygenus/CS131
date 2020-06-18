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
  (if (and (set! % #t) (eqv? (eval (expr-compare x y)) (eval x))
           (set! % #f) (eqv? (eval (expr-compare x y)) (eval y)))
      #t #f))

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
  (let ([nx (cons (blend-lambdas (car x) (car y)) (cdr x))]
        [ny (cons (blend-lambdas (car y) (car x)) (cdr y))])
    (if (or (not (eq? (list? (cadr nx)) (list? (cadr ny))))
             (and (and (list? (cadr nx)) (list? (cadr ny)))
                  (or (not (eqv? (car nx) (car ny))) (not (= (length (cadr nx)) (length (cadr ny)))))))
        (list 'if '%
              (list (car x) (cadr nx) (expr-compare (caddr nx) (caddr nx)))
              (list (car y) (cadr ny) (expr-compare (caddr ny) (caddr ny))))
        (cons (expr-unify (car nx) (car ny))
              (lambda-blend-params (cadr nx) (cddr nx) (cadr ny) (cddr ny))))))

; 'lambda && 'λ -> 'λ && 'λ
;
(define (blend-lambdas l1 l2)
  (if (or (and (eqv? l1 'lambda) (eqv? l2 'λ))
          (and (eqv? l1 'λ) (eqv? l2 'lambda)))
      'λ l1))

; map a function on a list OR perform once on atom
;
(define (map-opt fn x y)
  (if (and (list? x) (list? y))
      (map fn x y)
      (fn x y)))

; compares parameters for x and y, blending if necessary
; invariant: len params x = len params y
;
(define (lambda-blend-params x-params x-expr y-params y-expr)
  (let* ([merged-params (blend-names x-params y-params)]
         [x-expr (map-names (map-opt (lambda (x y) (cons x y)) x-params merged-params) x-expr)]
         [y-expr (map-names (map-opt (lambda (x y) (cons x y)) y-params merged-params) y-expr)])
    (cons merged-params (expr-compare x-expr y-expr))))

; if two symbols are equivalent but malnamed, blend them with '!'
;
(define (blend-names x y)
  (map-opt (lambda (x-param y-param)
             (if (eqv? x-param y-param) x-param
                 (string->symbol (string-append (symbol->string x-param)
                                                "!"
                                                (symbol->string y-param)))))
           x y))

; use a list of pairs TRANS to transform all symbols in expr
;
(define (map-names trans expr)
  (cond [(null? expr) '()]
        [(not (list? (car expr)))
         (let ([newval (assv (car expr) (if (list? trans) trans (list trans)))])
           (if newval
               (cons (cdr newval) (map-names trans (cdr expr)))
               (cons (car expr) (map-names trans (cdr expr)))))]
        [(or (eqv? (car (car expr)) 'lambda) (eqv? (car (car expr)) 'λ))
         (cons (map-lambda-expr trans (car expr)) (map-names trans (cdr expr)))]
        [#t  (cons (map-names trans (car expr)) (map-names trans (cdr expr)))]))

; perform variable mapping for a sub-lambda-expression
;
(define (map-lambda-expr trans expr)
   (let ([new-trans (key-remove trans (cadr expr))])
     (cons (car expr)
           (cons (map (lambda (e) (let ([pair (assv e new-trans)]) (if pair (cadr pair) e))) (cadr expr))
                 (map-names new-trans (cddr expr))))))

; remove all PAIRS with a key in KEYS
;
(define (key-remove pairs keys)
  (if (null? keys) pairs
      (let ([found (assv (car keys) pairs)])
        (if found
            (key-remove (remv found pairs) (cdr keys))
            (key-remove pairs (cdr keys))))))
            