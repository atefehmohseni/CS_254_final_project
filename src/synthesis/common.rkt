#lang rosette

(require "./data.rkt")
(provide answer-vars get-block group-blocks goal-diff)

(define (mk-target-var)
  (define-symbolic* b boolean?)
  b)

(define answer-vars
  (build-list (length addr-block-pairs) (lambda (_) (mk-target-var))))

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
