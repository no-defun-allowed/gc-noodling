#lang plai/gc2/collector

(require "pseudo-lru.rkt")
(define bump 0)

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
  (set! bump 0))

(define (ensure-room words more-roots)
  (when (> (+ bump words) (heap-size))
    (run-gc more-roots))
  (when (> (+ bump words) (heap-size))
    (error "Out of memory")))

;;; Allocation and heap layout noise.

;; FLAT mark value
(define (gc:alloc-flat val)
  (when (eq? val 'gc-misses)
    (set! val misses))
  (ensure-room 3 '())
  (let ([address bump])
    (theap-set! address 'flat)
    (theap-set! (+ address 1) #f)
    (theap-set! (+ address 2) val)
    (set! bump (+ bump 3))
    address))
(define (gc:flat? loc)
  (eq? 'flat (theap-ref loc)))
(define (gc:deref loc)
  (unless (eq? 'flat (theap-ref loc))
    (error "deref object not flat" loc))
  (theap-ref (+ loc 2)))

;; CONS mark first rest
(define (gc:cons first rest)
  (ensure-room 4 (list first rest))
  (let ([address bump])
    (theap-set! address 'cons)
    (theap-set! (+ address 1) #f)
    (theap-set! (+ address 2) (read-root first))
    (theap-set! (+ address 3) (read-root rest))
    (set! bump (+ bump 4))
    address))
(define (gc:cons? loc)
  (eq? 'cons (theap-ref loc)))

(define (gc:first loc)
  (unless (eq? 'cons (theap-ref loc))
    (error "first object not cons"))
  (theap-ref (+ loc 2)))
(define (gc:rest loc)
  (unless (eq? 'cons (theap-ref loc))
    (error "first object not cons"))
  (theap-ref (+ loc 3)))
(define (gc:set-first! loc value)
  (unless (eq? 'cons (theap-ref loc))
    (error "set-first! object not cons"))
  (theap-set! (+ loc 2) value))
(define (gc:set-rest! loc value)
  (unless (eq? 'cons (theap-ref loc))
    (error "set-rest! object not cons"))
  (theap-set! (+ loc 3) value))

;; CLOSURE mark code free-var-count free-vars ...
(define (gc:closure code free-vars)
  (ensure-room (+ 4 (length free-vars)) free-vars)
  (let* ([address bump]
         [end-address (+ address 4)])
    (theap-set! address 'closure)
    (theap-set! (+ 1 address) #f)
    (theap-set! (+ 2 address) code)
    (theap-set! (+ 3 address) (length free-vars))
    (for ([var free-vars]
          [position (in-naturals 4)])
      (theap-set! (+ address position) (read-root var))
      (set! end-address (add1 end-address)))
    (set! bump end-address)
    address))

(define (gc:closure-code-ptr loc)
  (unless (eq? 'closure (theap-ref loc))
    (error "closure-code-ptr object not closure"))
  (theap-ref (+ 2 loc)))
(define (gc:closure-env-ref loc i)
  (unless (eq? 'closure (theap-ref loc))
    (error "closure-env-ref object not closure"))
  (theap-ref (+ 4 loc i)))
(define (gc:closure? loc)
  (eq? 'closure (theap-ref loc)))

;;; Finally some GC
(define next-bump #f)
(define compact-count 0)

(define (run-gc additional-roots)
  (set! compact-count (add1 compact-count))
  (display (format "Compacting (#~a)" compact-count))
  (map mark-root (get-root-set))
  (map mark-root additional-roots)
  (compute-locations)
  (update-references)
  (map update-root (get-root-set))
  (map update-root additional-roots)
  (relocate)
  (displayln (format " ~s -> ~s (~s misses)"
                     bump next-bump misses))
  (set! bump next-bump))

(define (mark-root root) (mark (read-root root)))

(define (map-pointers f loc)
  (define (%map start length)
    (for ([n (in-range start (+ start length))])
      (f (+ loc n))))
  (case (theap-ref loc)
    [(flat) #t] ; no pointers
    [(cons) (%map 2 2)]
    [(closure) (%map 4 (theap-ref (+ loc 3)))]
    [else (error "Unknown object at " loc)]))

(define (mark loc)
  (define (%mark loc) (mark (theap-ref loc)))
  (unless (theap-ref (add1 loc)) ; already marked
    (theap-set! (add1 loc) #t) ; mark object
    (map-pointers %mark loc)))

(define (size loc)
  (case (theap-ref loc)
    [(flat) 3]
    [(cons) 4]
    [(closure) (+ 4 (theap-ref (+ loc 3)))]
    [else (error "Unknown object at " loc)]))

(define-syntax-rule (while test body ...)
  (let loop ()
    (when test
      body ...
      (loop))))

(define (compute-locations)
  (let ([scan 0]
        [free 0])
    (while (< scan bump)
      (when (theap-ref (+ scan 1)) ; is marked?
        (theap-set! (+ scan 1) free)
        (set! free (+ free (size scan))))
      (set! scan (+ scan (size scan))))
    (set! next-bump free)))

(define (update-references)
  (let ([scan 0])
    (while (< scan bump)
      (when (theap-ref (+ scan 1)) ; is marked?
        (map-pointers (lambda (loc)
                        ;; replace with address in forwarding pointer
                        (theap-set! loc (theap-ref (add1 (theap-ref loc)))))
                      scan))
      (set! scan (+ scan (size scan))))))
(define (update-root root)
  ;; replace root with its forwarding pointer
  (set-root! root (theap-ref (add1 (read-root root)))))

(define (relocate)
  (let ([scan 0])
    (while (< scan bump)
      ;; grab the size before, because we could clobber the object at
      ;; SCAN with MOVE. This is a bug in the GC Handbook that will be
      ;; fixed in the 2022 edition.
      (let ([s (size scan)])
        (when (theap-ref (+ scan 1)) ; is marked?
          (let ([to (theap-ref (+ scan 1))])
            (unless (<= to scan)
              (error "Object moving the wrong way at" scan))
            (move scan to)
            (theap-set! (+ to 1) #f))) ; unset mark
        (set! scan (+ scan s))))))

(define (move from to)
  (for ([n (in-range (size from))])
    (theap-set! (+ to n) (theap-ref (+ from n)))))
