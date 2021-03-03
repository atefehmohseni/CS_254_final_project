#lang rosette/safe

(require "./config.rkt")
(require rosette/lib/synthax)
(require rosette/lib/angelic)

(define hash-indices (mk-asc-ints routing-bits))

(define (twiddle-bits b1 b2)
  ([choose bvand bvor bvxor] b1 b2))

;(define (operate bits)
;  (map (lambda (b) (twiddle-bits b (apply choose* bits))) bits))

; need some macros to generate this automatically...
(define (operate bits)
  (list
   (twiddle-bits (list-ref bits 0) (list-ref bits [choose 0 1 2 3]))
   (twiddle-bits (list-ref bits 1) (list-ref bits [choose 0 1 2 3]))
   (twiddle-bits (list-ref bits 2) (list-ref bits [choose 0 1 2 3]))
   (twiddle-bits (list-ref bits 3) (list-ref bits [choose 0 1 2 3]))
   ))

(define sample-bits (bitvector->bits (bv 9 4)))

; select bits b0 .. bn, twiddle them to b0' .. bn',
; then compute (b0' .. bn') % total cache lines
(define (hash-alg addr)
  (let* ((bits (map (lambda (i) (bit i addr)) hash-indices))
         (bits2 (operate bits))
         (cache-index (apply concat bits2)))
    (bvsmod cache-index (bv total-cache-lines routing-bits))))


; by default, these variables are existentially quantified
(define-symbolic x (bitvector routing-bits))

(define result
  (synthesize
 #:forall (list x)
 #:guarantee (assert (equal? (hash-alg x) (bv 0 routing-bits)))))

(when (solution? result)
  (print result)
  (print-forms result))