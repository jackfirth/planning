#lang racket/base

(require racket/contract/base)

(provide
 (contract-out
  [default (-> any/c any/c any/c)]
  [sequence->set (-> (sequence/c any/c) set?)]
  [set-contains? (-> set? any/c boolean?)]
  [set-contains-all? (-> set? set? boolean?)]
  [set-contains-any? (-> set? set? boolean?)]
  [set-contains-none? (-> set? set? boolean?)]
  [hash-contains-none? (-> hash? (sequence/c entry?) boolean?)]
  [hash-put (-> immutable-hash? any/c any/c immutable-hash?)]
  [hash-put-all (-> immutable-hash? hash? immutable-hash?)]
  [hash-remove-all (-> immutable-hash? (sequence/c any/c) immutable-hash?)]
  [multiset-remove-all (-> multiset? (sequence/c any/c) multiset?)]
  [multiset-set-all-frequencies
   (-> multiset? (hash/c any/c exact-nonnegative-integer? #:immutable #t)
       multiset?)]))

(module+ doc
  (provide
   (contract-out
    [reference-tech (-> pre-content? ... element?)]
    [rebellion-tech (-> pre-content? ... element?)]
    [make-module-sharing-evaluator-factory
     (->* ()
          (#:private (listof module-path?)
           #:public (listof module-path?))
          (-> (-> any/c any)))])))

(require racket/set
         racket/sequence
         rebellion/collection/entry
         rebellion/collection/hash
         rebellion/collection/list
         rebellion/collection/multiset)

(module+ test
  (require (submod "..")
           rackunit))

(module+ doc
  (require scribble/core
           scribble/decode
           scribble/example
           scribble/manual))

;@------------------------------------------------------------------------------

(define (default arg default-value)
  (if (unsupplied-arg? arg) default-value arg))

(define (sequence->set seq) (for/set ([v seq]) v))

(define (set-contains? s item) (set-member? s item))
(define (set-contains-all? s items) (subset? items s))
(define (set-contains-any? s items) (not (set-contains-none? s items)))

(define (set-contains-none? s items)
  (equal? (set-count (set-subtract s items)) (set-count s)))

(define (hash-contains? h k v)
  (and (hash-has-key? h k) (equal? (hash-ref h k) v)))

(define (hash-contains-none? h entries)
  (for/and ([e entries])
    (not (hash-contains? h (entry-key e) (entry-value e)))))

(define (hash-put h k v) (hash-set h k v))

(define (hash-put-all h entries)
  (for/fold ([h h]) ([(k v) (in-hash entries)]) (hash-put h k v)))

(define (hash-remove-all h keys)
  (for/fold ([h h]) ([k keys]) (hash-remove h k)))

(module+ test
  (test-case "hash-put"
    (check-equal? (hash-put (hash 'a 1) 'b 2) (hash 'a 1 'b 2))
    (check-equal? (hash-put (hash 'a 1) 'a 2) (hash 'a 2)))

  (test-case "hash-put-all"
    (define h (hash 'a 1 'b 2))
    (check-equal? (hash-put-all h (hash 'c 3 'd 4)) (hash 'a 1 'b 2 'c 3 'd 4)))
  
  (test-case "hash-remove-all"
    (define h (hash 'a 1 'b 2 'c 3))
    (check-equal? (hash-remove-all h (list 'a 'c)) (hash 'b 2))))

(define (multiset-remove-all set elements)
  (for/fold ([set set]) ([e elements]) (multiset-remove set e)))

(define (multiset-set-all-frequencies set frequencies)
  (for/fold ([set set]) ([(v freq) (in-immutable-hash frequencies)])
    (multiset-set-frequency set v freq)))

(module+ doc
  (define (reference-tech . text)
    (apply tech #:doc '(lib "scribblings/reference/reference.scrbl") text))

  (define (rebellion-tech . text)
    (apply tech #:doc '(lib "rebellion/main.scrbl") text))

  (define (make-module-sharing-evaluator-factory
           #:public [public-modules empty-list]
           #:private [private-modules empty-list])
    (define base-factory
      (make-base-eval-factory (append private-modules public-modules)))
    (λ ()
      (define evaluator (base-factory))
      (evaluator `(require ,@public-modules))
      evaluator)))
