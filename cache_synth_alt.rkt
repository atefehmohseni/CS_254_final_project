
#lang rosette
(require rosette/lib/angelic  ; provides `choose*`
         rosette/lib/match)   ; provides `match`
(require racket/random)

;; come up with a mapping of addresses to identifiers assuming
;; tile1 has a 4 line cache, tile2 has a 2 line cache, and so on
;;
;; ideally, you could just check if an addr is within some range,
;; map each cache to a proportionate range, and then send the addr
;; to its appropriate cache
;;
;; in practice this is dispreferred because strides would be mapped
;; to the same cacheline which would create a hotspot and it
;; requires expensive comparators
;;
;; in practice however it's a place to start;
;; give 8 bits to a memory address
(define bv0 (bv 0 8))
(define bv128 (bv 128 8))
(define bv192 (bv 192 8))
(define bv224 (bv 224 8))

(define (hash-ideal addr)
   (cond
    [(and (bvugt addr bv0) (bvult addr bv128)) 1]
    [(and (bvugt addr bv128) (bvult addr bv192)) 2]
    [(and (bvugt addr bv192) (bvult addr bv224)) 3]
    [else 4]
    )
  )

;; create a simple interpreter that can choose
;; from several bitvector operations
(struct XOR (left right) #:transparent)
(struct AND (left right) #:transparent)
(struct OR (left right) #:transparent)
(struct SHL (left right) #:transparent)
(struct LSHR (left right) #:transparent)
(struct ASHR (left right) #:transparent)
(struct NOT (arg) #:transparent)

;; (struct EQ (left right) #:transparent)
;; (struct SLT (left right) #:transparent)
;; (struct ULT (left right) #:transparent)
;; (struct SLE (left right) #:transparent)
;; (struct ULE (left right) #:transparent)
;; (struct SGT (left right) #:transparent)
;; (struct UGT (left right) #:transparent)
;; (struct SGE (left right) #:transparent)
;; (struct UGE (left right) #:transparent)

;; (struct ADD (left right) #:transparent)
;; (struct SUB (left right) #:transparent)
;; (struct MUL (left right) #:transparent)
;; (struct SDIV (left right) #:transparent)
;; (struct UDIV (left right) #:transparent)
;; (struct SREM (left right) #:transparent)
;; (struct SMOD (left right) #:transparent)
;; (struct NEG (arg) #:transparent)

;; (struct CONCAT (LEFT RIGHT) #:transparent)
;; (struct EXTRACT (LEFT RIGHT) #:transparent)
;; (struct SEXTEND (LEFT RIGHT) #:transparent)
;; (struct ZEXTEND (LEFT RIGHT) #:transparent)

(define (interpret p)
  (match p
    [(XOR a b)    (bvxor (interpret a) (interpret b))]
    [(AND a b)    (bvand (interpret a) (interpret b))]
    [(OR a b)     (bvor (interpret a) (interpret b))]
    [(SHL a b)    (bvshl (interpret a) (interpret b))]
    [(LSHR a b)   (bvlshr (interpret a) (interpret b))]
    [(ASHR a b)   (bvashr (interpret a) (interpret b))]
    [(NOT a)      (bvnot (interpret a))]
    [_ p]))

;; concrete example
(define prog (XOR (bv 4 7) (bv 7 7)))
(interpret prog)                            ;; (bv #b0000011 7)
(hash-ideal (bv 4 8))                       ;; 1

;; symbolic example
(define-symbolic y (bitvector 8))
(interpret (XOR (bv 4 8) y))                ;; (bvxor (bv #x04 8) y)

;; synthesize a function that has the same behavior as hash-ideal
;; but using only bitvector operations
(define (??expr terminals)
  (define left (apply choose* terminals))
  (define right(apply choose* terminals))
  (choose*   (XOR left right)
             (AND left right)
             (OR left right) 
             (SHL left right)
             (LSHR left right)
             (ASHR left right)
             (NOT left) 
             left) 
  )

(define-symbolic x (bitvector 8))
(define-symbolic p q (bitvector 8))
(define sketch
  (XOR (??expr (list x p q)) (??expr (list x p q))))

(displayln (hash-ideal x))

;; ;; ((interpret sketch) (bv 0 8)) |--> (XOR (XOR (bv #xff 8) (bv #xff 8)) (NOT (bv #xff 8)))
;; ;; ((interpret sketch) (bv 1 8)) |--> (XOR (NOT (bv #x03 8)) (ASHR (bv #xe8 8) (bv #x03 8)))
(define M
  (synthesize
   #:forall (list x)
   ;;#:guarantee (assert (equal? (interpret sketch) (hash-ideal x)))))
   #:guarantee (assert (equal? (interpret sketch) (bv 1 8)))))

(evaluate sketch M)
