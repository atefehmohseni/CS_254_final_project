#lang racket

(provide
 word-size cache-lines total-cache-lines routing-bits
 mk-asc-ints)

; configuration
(define word-size 16)
(define cache-lines (list 8 4 4))
(define total-cache-lines (apply + cache-lines))
(define routing-bits (exact-ceiling (log total-cache-lines 2)))


(define (mk-asc-ints size) (build-list size values))
