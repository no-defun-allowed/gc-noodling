#lang plai/gc2/collector

(define bump 0)

(define (init-allocator)
  (set! bump 0))

(define (gc:deref loc)
  (unless (eq? 'flat (heap-ref loc))
    (error "deref object not flat" loc))
  (heap-ref (add1 loc)))

(define (gc:alloc-flat val)
  (let ([address bump])
    (heap-set! address 'flat)
    (heap-set! (add1 address) val)
    (set! bump (+ bump 2))
    address))
(define (gc:flat? loc)
  (eq? 'flat (heap-ref loc)))

(define (gc:cons first rest)
  (let ([address bump])
    (heap-set! address 'cons)
    (heap-set! (+ address 1) (read-root first))
    (heap-set! (+ address 2) (read-root rest))
    (set! bump (+ bump 3))
    address))
(define (gc:cons? loc)
  (eq? 'cons (heap-ref loc)))

(define (gc:first loc)
  (unless (eq? 'cons (heap-ref loc))
    (error "first object not cons"))
  (heap-ref (add1 loc)))
(define (gc:rest loc)
  (unless (eq? 'cons (heap-ref loc))
    (error "first object not cons"))
  (heap-ref (+ 2 loc)))
(define (gc:set-first! loc value)
  (unless (eq? 'cons (heap-ref loc))
    (error "set-first! object not cons"))
  (heap-set! (add1 loc) value))
(define (gc:set-rest! loc value)
  (unless (eq? 'cons (heap-ref loc))
    (error "set-rest! object not cons"))
  (heap-set! (+ 2 loc) value))

(define (gc:closure code free-vars)
  (let* ([address bump]
         [end-address (+ address 2)])
    (heap-set! address 'closure)
    (heap-set! (+ 1 address) code)
    (for ([var free-vars]
          [position (in-naturals 2)])
      (heap-set! (+ address position) (read-root var))
      (set! end-address (add1 end-address)))
    (set! bump end-address)
    address))

(define (gc:closure-code-ptr loc)
  (unless (eq? 'closure (heap-ref loc))
    (error "closure-code-ptr object not closure"))
  (heap-ref (+ 1 loc)))
(define (gc:closure-env-ref loc i)
  (unless (eq? 'closure (heap-ref loc))
    (error "closure-env-ref object not closure"))
  (heap-ref (+ 2 loc i)))
(define (gc:closure? loc)
  (eq? 'closure (heap-ref loc)))