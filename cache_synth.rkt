#lang rosette/safe

(require
 "./config.rkt"
 "./data.rkt"
 "./common.rkt"
  rosette/lib/synthax
  rosette/lib/angelic
  (prefix-in racket: racket/base)
  (only-in racket/list group-by)
  racket/match
  )

(define hash-indices (mk-asc-ints word-size))


(define sample-bits (bitvector->bits (bv 9 4)))

(define-synthax (twiddle-hole b1 b2 depth)
  #:base (choose b1 b2)
  #:else (choose
          b1 b2
          ((choose bvand bvor bvxor) (twiddle-hole b1 b2 (- depth 1))
                                     (twiddle-hole b1 b2 (- depth 1)))))

; select bits b0 .. bn, twiddle them to b0' .. bn',
; then compute (b0' .. bn') % total cache lines
(define (hash-alg addr)
  (define (operate bits)
    ; need some macros to generate this automatically...
    (list
     (twiddle-hole (list-ref bits (??)) (list-ref bits (??)) 2)
     (twiddle-hole (list-ref bits (??)) (list-ref bits (??)) 2)
     (twiddle-hole (list-ref bits (??)) (list-ref bits (??)) 2)
     (twiddle-hole (list-ref bits (??)) (list-ref bits (??)) 2)
     ))

  (let* ((bits (map (lambda (i) (bit i addr)) hash-indices))
         (bits2 (operate bits))
         (cache-index (apply concat bits2)))
    ; (bvsmod cache-index (bv total-cache-lines routing-bits))
    cache-index
     ))


; by default, these variables are existentially quantified
(define-symbolic x (bitvector routing-bits))

(define test-asserts
  (racket:for/list
   ([t test-cases] [b answer-vars])
   (equal? b (equal? (hash-alg (first t)) (bv (second t) routing-bits)))))

(define synth-property
  (racket:foldl (lambda (p q) (and p q)) #t test-asserts))

(define cutoffs (cumulative-sum (reverse (cdr (reverse cache-lines)))))
(define exp-dist (map (lambda (n) (/ (* n (length test-cases)) (apply + cache-lines))) cache-lines))

; test case based objective
(define result
  (optimize
   #:maximize (racket:map (lambda (b) (if b 1 0)) answer-vars)
   #:guarantee (assert synth-property)))

; difference minimization (very slow!!!)
; (define result
;   (let ((output (map (lambda (t) (bitvector->natural (hash-alg (first t)))) test-cases)))
;     (optimize
;      #:minimize (goal-diff (group-blocks cutoffs output) exp-dist)
;      #:guarantee (assert #t))))

; dummy for testing purposes
; (define result
;   (synthesize
;    #:forall '()
;    #:guarantee (assert #f)))

(racket:define-namespace-anchor ns-anc)
(define ns (racket:namespace-anchor->namespace ns-anc))

(if (sat? result)
    (begin
      ; (print result)
      ; (print-forms result)

      ; retrieve and eval hash-alg
      (define hash-alg-sols
        (racket:for/list ([f (generate-forms result)])
          (racket:syntax-case f ()
             [(_ (name _) _ ...)
              (if (racket:free-transformer-identifier=? #'hash-alg #'name)
                  f
                  #f)]
             [_ #f])))
      (print-forms result)
      (racket:eval-syntax (first hash-alg-sols) ns)

      ; get actual hashes
      (define actual (map (lambda (t) (bitvector->natural (hash-alg (first t)))) test-cases))
      (printf "actual hashes: ~a\n" actual)

      ; compute accuracy
      (define correct
        (count identity
         (racket:for/list ([h actual] [t test-cases])
                          (racket:equal? h (second t)))))
      (printf "correct (exactly) bin: ~a/~a\n" correct (length test-cases))

      ; compute homed caches
      (define actual-bins (map (curry get-block cutoffs) actual))
      (printf "actual distr: ~a\n" (map length (group-by identity (sort actual-bins <))))
      (printf "expected distr: ~a\n"
              (map (lambda (n) (/ (* n (length test-cases)) (apply + cache-lines))) cache-lines))
      )
    (println "no solution"))
