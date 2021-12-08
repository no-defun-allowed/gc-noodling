#lang plai/gc2/collector

(require "pseudo-lru.rkt")
(define bump 0)
(define half-end #f)
(define half-start #f)

(define lru (make-lru 4))
(define misses 0)

(define (theap-set! loc value)
  (unless (lru-member?! lru loc)
    (set! misses (add1 misses)))
  (heap-set! loc value))
(define (theap-ref loc)
  (unless (lru-member?! lru loc)
    (set! misses (add1 misses)))
  (heap-ref loc))

(define (init-allocator)
  (set! bump 0)
  (set! half-start 0)
  (set! half-end (/ (heap-size) 2)))

(define (ensure-room words more-roots)
  (when (> (+ bump words) half-end)
    (run-gc more-roots))
  (when (> (+ bump words) half-end)
    (error "Out of memory")))

;;; Allocation and heap layout noise.

(define (gc:deref loc)
  (unless (eq? 'flat (theap-ref loc))
    (error "deref object not flat" loc))
  (theap-ref (add1 loc)))

(define (gc:alloc-flat val)
  (when (eq? val 'gc-misses)
    (set! val misses))
  (ensure-room 2 '())
  (let ([address bump])
    (theap-set! address 'flat)
    (theap-set! (add1 address) val)
    (set! bump (+ bump 2))
    address))
(define (gc:flat? loc)
  (eq? 'flat (theap-ref loc)))

(define (gc:cons first rest)
  (ensure-room 3 (list first rest))
  (let ([address bump])
    (theap-set! address 'cons)
    (theap-set! (+ address 1) (read-root first))
    (theap-set! (+ address 2) (read-root rest))
    (set! bump (+ bump 3))
    address))
(define (gc:cons? loc)
  (eq? 'cons (theap-ref loc)))

(define (gc:first loc)
  (unless (eq? 'cons (theap-ref loc))
    (error "first object not cons"))
  (theap-ref (add1 loc)))
(define (gc:rest loc)
  (unless (eq? 'cons (theap-ref loc))
    (error "first object not cons"))
  (theap-ref (+ 2 loc)))
(define (gc:set-first! loc value)
  (unless (eq? 'cons (theap-ref loc))
    (error "set-first! object not cons"))
  (theap-set! (add1 loc) value))
(define (gc:set-rest! loc value)
  (unless (eq? 'cons (theap-ref loc))
    (error "set-rest! object not cons"))
  (theap-set! (+ 2 loc) value))

(define (gc:closure code free-vars)
  (ensure-room (+ 3 (length free-vars)) free-vars)
  (let* ([address bump]
         [end-address (+ address 3)])
    (theap-set! address 'closure)
    (theap-set! (+ 1 address) code)
    (theap-set! (+ 2 address) (length free-vars))
    (for ([var free-vars]
          [position (in-naturals 2)])
      (theap-set! (+ address position) (read-root var))
      (set! end-address (add1 end-address)))
    (set! bump end-address)
    address))

(define (gc:closure-code-ptr loc)
  (unless (eq? 'closure (theap-ref loc))
    (error "closure-code-ptr object not closure"))
  (theap-ref (+ 1 loc)))
(define (gc:closure-env-ref loc i)
  (unless (eq? 'closure (theap-ref loc))
    (error "closure-env-ref object not closure"))
  (theap-ref (+ 3 loc i)))
(define (gc:closure? loc)
  (eq? 'closure (theap-ref loc)))

;;; Finally some GC

(define to-scan #f)

(define (run-gc more-roots)
  (display "Copying")
  (let ([old-bump bump]
        [old-start half-start])
    (flip)
    (set! to-scan half-start)
    (map copy-root (get-root-set))
    (map copy-root more-roots)
    (let loop ()
      (when (< to-scan bump)
        (scan to-scan)
        (loop)))
    (displayln (format " ~s -> ~s (~s misses)"
                       (- old-bump old-start)
                       (- bump half-start)
                       misses))))

(define (copy-root root)
  (set-root! root (copy (read-root root))))

(define (copy loc)
  (define (%copy length)
    (let ([start bump])
      (set! bump (+ bump length))
      (for ([n (in-range length)])
        (theap-set! (+ start n) (theap-ref (+ loc n))))
      (theap-set! loc 'forward)
      (theap-set! (+ 1 loc) start)
      start))
  (case (theap-ref loc)
    [(forward) (theap-ref (add1 loc))]
    [(flat) (%copy 2)]
    [(cons) (%copy 3)]
    [(closure) (%copy (+ 3 (theap-ref (+ 2 loc))))]
    [else (error "Unknown object at" loc)]))

(define (scan loc)
  (define (%skip n)
    (set! to-scan (+ to-scan n)))
  (define (%scan start end)
    (for ([n (in-range start end)])
      (theap-set! (+ loc n) (copy (theap-ref (+ loc n))))))
  (case (theap-ref loc)
    [(forward) (error "Shouldn't have forwarding pointer in tospace")]
    [(flat) (%skip 2)]
    [(cons) (%scan 1 3) (%skip 3)]
    [(closure)
     (let ([end (+ 3 (theap-ref (+ 2 loc)))])
       (%scan 3 end)
       (%skip end))]
    [else (error "Unknown object at" loc)]))

(define (flip)
  (cond
    [(zero? half-start)
     (set! half-start (/ (heap-size) 2))
     (set! half-end (heap-size))]
    [else
     (set! half-start 0)
     (set! half-end (/ (heap-size) 2))])
  (set! bump half-start))
