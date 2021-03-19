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

(define-grammar (twiddle-hole b1 b2)
  [expr (choose b1 b2 ((op) (expr) (expr)))]
  [op (choose bvand bvor bvxor)])

(current-grammar-depth hole-depth)

; select bits b0 .. bn, twiddle them to b0' .. bk',
(define (hash-alg addr)
  (define (operate bits)
    (list
     (twiddle-hole (list-ref bits (??)) (list-ref bits (??)))
     (twiddle-hole (list-ref bits (??)) (list-ref bits (??)))
     (twiddle-hole (list-ref bits (??)) (list-ref bits (??)))
     (twiddle-hole (list-ref bits (??)) (list-ref bits (??)))
     ; (twiddle-hole (list-ref bits (??)) (list-ref bits (??)))
     ))

  (let* ((bits (map (lambda (i) (bit i addr)) hash-indices))
         (bits2 (operate bits))
         (block (apply concat bits2)))
    block))

(define answer-vars (mk-answer-vars (length test-cases)))

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
             [(_ (name args) body ...)
              (if (racket:free-transformer-identifier=? #'hash-alg #'name)
                  #'(lambda (args) body ...)
                  #f)]
             [_ #f])))
      (print-forms result)
      (define synth-alg
        (racket:eval (racket:syntax->datum (first hash-alg-sols)) ns))

      ; get actual hashes
      (define actual (map (lambda (t) (bitvector->natural (synth-alg (first t)))) test-cases))
      (printf "actual blocks: ~a\n" actual)

      ; compute accuracy
      (define correct
        (count identity
         (racket:for/list ([h actual] [t test-cases])
                          (racket:equal? h (second t)))))
      (printf "# blocks exactly matching: ~a/~a\n" correct (length test-cases))

      ; compute homed caches
      (define actual-bins (map (curry get-block cutoffs) actual))
      (define actual-distr
        (map length (group-by identity (sort actual-bins <))))
      (define expected-distr
        (map
         (lambda (n) (/ (* n (length test-cases)) (apply + cache-lines)))
         cache-lines))
      (printf "actual distr: ~a\n" actual-distr)
      (printf "expected distr: ~a\n" expected-distr)
      (printf "% difference: ~a%\n"
              (let ((deviation
                     (apply + (goal-diff actual-distr expected-distr)))
                    (total (length test-cases)))
                (exact->inexact (* 100 (/ deviation total)))))
      )
    (printf "no solution\n"))
