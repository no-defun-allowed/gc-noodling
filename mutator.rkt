#lang plai/gc2/mutator
(allocator-setup "lisp2.rkt" 680)

(define (deriv-symbol e x)
  (if (symbol=? e x) 1 0))

(define (not e) (if e #f #t))
(define (plus a b)
  (cond
    ; The GC2 "FFI" requires that I test CONS? first, cause NUMBER?
    ; calls a primop and tries to deref a cons cell anyway.
    [(and (not (cons? a)) (number? a) (= a 0)) b]
    [(and (not (cons? b)) (number? b) (= b 0)) a]
    [else (cons '+ (cons a (cons b empty)))]))
(define (times a b)
  (cond
    ; The GC2 "FFI" requires that I test CONS? first, cause NUMBER?
    ; calls a primop and tries to deref a cons cell anyway.
    [(and (not (cons? a)) (number? a) (= a 1)) b]
    [(and (not (cons? b)) (number? b) (= b 1)) a]
    [else (cons '* (cons a (cons b empty)))]))

(define (deriv-cons e x)
  (cond
    [(symbol=? (first e) '+)
     (plus (deriv (first (rest e)) x)
           (deriv (first (rest (rest e))) x))]
    [(symbol=? (first e) '*)
     (let* ([u (first (rest e))]
            [v (first (rest (rest e)))]
            [du (deriv u x)]
            [dv (deriv v x)])
       (plus (times du v) (times u dv)))]))

(define (deriv e x)
  (cond
    [(cons? e) (deriv-cons e x)]
    [(symbol? e) (deriv-symbol e x)]
    [(number? e) 0]))

(let* ([d1 (deriv '(+ (+ (* x (* x x)) (* x x)) x) 'x)]
       [d2 (deriv d1 'x)]
       [d3 (deriv d2 'x)])
  (plus d1 (plus d2 d3)))