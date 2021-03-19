#lang rosette

(require "./data.rkt" "./config.rkt")
(provide mk-answer-vars get-block group-blocks goal-diff)

(define (mk-target-var)
  (define-symbolic* b boolean?)
  b)

(define (mk-answer-vars n)
  (build-list n (lambda (_) (mk-target-var))))

(define (get-block-helper cutoffs n i)
  (cond
    [(empty? cutoffs) n]
    [(> (car cutoffs) i) n]
    [else (get-block-helper (cdr cutoffs) (+ n 1) i)]))

(define (get-block cutoffs i)
  (get-block-helper cutoffs 0 i))

(define (group-blocks cutoffs idxs)
  (for/fold
      ([grps (build-list (+ (length cutoffs) 1) (lambda (_) 0))])
      ([i idxs])
    (define b (get-block cutoffs i))
    (define-values (g1 g2) (split-at grps b))
    (append g1 (cons (+ 1 (car g2)) (cdr g2)))
  ))

(define (goal-diff actual-count expected-count)
  (for/list ([a actual-count] [e expected-count])
    (abs (- a e))))

(define (get-bins config pairs)
  (define cutoffs (cumulative-sum config))
  (map length
       (group-by identity (sort (map (lambda (t) (get-block cutoffs (cdr t))) pairs) <))))

(define (mk-random-data-for-bin n)
  (define expected-distr
    (map
     (lambda (x) (/ (* x n) (apply + cache-lines)))
     cache-lines))
  (define (go cur)
    (cond
      [(equal? (get-bins cache-lines cur) expected-distr) cur]
      [#t (go (mk-random-data n))]))
  (go '()))
