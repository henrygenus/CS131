#lang racket
(provide (all-defined-out))

; variables for testing of expr-compare
; USAGE > (test-expr-compare (test-expr x) (test-expr y)
;
; (define test-expr-x )
; (define test-expr-y )

; function which calls expr-compare and evaluates results for %/{#t, #f)
; comparing the former to x and the latter to y
;
; (define test-expr-compare x y)

; compares two Scheme expressions x and y,
; produces a difference summary of the two
;
(define expr-compare
  (lambda (x y)
 (expr-compare-helper x y)))
; if next one is macro, need an extra paren

(define expr-compare-helper
   (lambda(x y)
    (cond [(and (null? x) (null? y)) '()]
          [(not (and (list? x) (list? y))) (arg-unify x y)]
          [(not (eq? (null? (cdr x)) (null? (cdr y)))) (list 'if '% x y)]
          [(and (eq? (car x) 'quote) (eq? (car y) 'quote))
           (if (eq? (cdr x) (cdr y)) x (list 'if '% x y))]
          [(or (member (car x) '(if lambda λ quote)) 
               (member (car y) '(if lambda λ quote)))
           (expr-macro-compare x y)]
          [#t (expr-fun-compare x y)]
          )))
  
(define expr-fun-compare
 (lambda(x y)
   (let ([prefix (expr-compare-helper (car x) (car y))]
         [rest (expr-compare-helper (cdr x) (cdr y))])
     (null? rest) prefix (cons prefix rest))))
   
; find a most general unification for lists x and y
; if (at-functor) we consider 'if 'lambda 'λ 'quote ' as macros
;
(define expr-macro-compare
  (lambda (x y)
    (if (not (eq? (car x) (car y)))
        (let ([rest-x (expr-compare-helper (cdr x) (cdr x))]
              [rest-y (expr-compare-helper (cdr y) (cdr y))])
          (list 'if '%
                (if (null? rest-x) (car x) (cons (car x) rest-x))
                (if (null? rest-y) (car y) (cons (car y) rest-y))))
        (expr-fun-compare x y))))
        
; finds a single binding which matches both X and Y
; this may be a conditional binding dependent on '%
;
(define arg-unify
  (lambda (x y)
    (cond [(eq? x y) x]
          [(or (boolean? x) (boolean? y)) (arg-match-bool x y)]
          [#t (arg-match x y)])))

; finds single binding for both X and Y
; this merges (lambda|λ) to λ and ('|quote) to '
; if X and Y are unequal, return ('if '% X Y); else return X
;
(define arg-match
  (lambda (x y)
    (cond [(or (and (eq? x "lambda") (eq? y "λ"))
               (and (eq? x "λ") (eq? y "lambda")))
           '(λ)]
          [(or (and (eq? x 'quote) (eq? y "'"))
               (and (eq? x "'") (eq? y 'quote)))
           "'"]
          [(eq? x y) x]
          [#t (list 'if '% x y)])))

; merge booleans before finding a single binding which matches both X and Y
; transforms: {#t & #t ->#t, #f & #f -> #f, #t & #f -> '%, #f & #t -> '(not %)}
; second level: {y/#t -> '(not %), y/#f -> '%, x/#t -> '%, x/#f -> '(not%)}
;
(define arg-match-bool
  (lambda (x y)
    (cond [(and (eq? x #t) (eq? y #f)) '%]
          [(and (eq? x #f) (eq? y #t)) '(not %)]
          [#t (let ([x (cond [(eq? x #t) '%] [(eq? x #f) '(not %)] [#t x])]
                    [y (cond [(eq? y #t) '(not %)] [(eq? y #f) '%] [#t y])])
                (arg-match x y))])))
