#lang racket

(provide make-lru lru-member?!)

(define cache-line-size 8)              ; 8 * 8 byte lines?

(struct lru
  ([timestamp #:mutable]
   timestamp-vector
   element-vector))

(define (make-lru size)
  (lru 0
       (make-vector size #f)
       (make-vector size #f)))

(define (position-to-evict timestamps)
  (for/fold ([oldest-time 1e100]
             [oldest-position #f])
            ([time (in-vector timestamps)]
             [position (in-naturals)])
    (if (< time oldest-time)
        (values time position)
        (values oldest-time oldest-position))))

(define (%lru-member?! lru element)
  (cond
    ;; Cache hit
    [(vector-member element (lru-element-vector lru)) #t]
    ;; Cache miss
    ;; Is there a new position to fill?
    [(vector-member #f (lru-element-vector lru))
     => (lambda (p)
          (vector-set! (lru-element-vector lru) p element)
          (vector-set! (lru-timestamp-vector lru) p (lru-timestamp lru))
          (set-lru-timestamp! lru (add1 (lru-timestamp lru)))
          #f)]
    [else
     (let-values ([(_ p) (position-to-evict (lru-timestamp-vector lru))])
       (vector-set! (lru-element-vector lru) p element)
       (vector-set! (lru-timestamp-vector lru) p (lru-timestamp lru))
       (set-lru-timestamp! lru (add1 (lru-timestamp lru)))
       #f)]))
       
(define (lru-member?! lru loc)
  (%lru-member?! lru (floor (/ loc cache-line-size))))
