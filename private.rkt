#lang racket/base

(require racket/contract/base)

(provide
 define-external-pict
 (contract-out
  [default (-> any/c any/c any/c)]
  [set-coercible-sequence/c contract?]
  [sequence->set (-> set-coercible-sequence/c set?)]
  [set-contains? (-> set? any/c boolean?)]
  [set-contains-all? (-> set? set-coercible-sequence/c boolean?)]
  [set-contains-any? (-> set? set-coercible-sequence/c boolean?)]
  [set-contains-none? (-> set? set-coercible-sequence/c boolean?)]
  [set-add-all (-> set? set-coercible-sequence/c set?)]
  [set-remove-all (-> set? set-coercible-sequence/c set?)]
  [hash-contains? (-> hash? any/c any/c boolean?)]
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

(require pict
         racket/runtime-path
         racket/set
         racket/sequence
         rebellion/collection/entry
         rebellion/collection/hash
         rebellion/collection/list
         rebellion/collection/multiset
         syntax/parse/define)

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

(define set-coercible-sequence/c
  (or/c set? list? multiset? vector? (sequence/c any/c)))

(define (sequence->set seq) (if (set? seq) seq (for/set ([v seq]) v)))

(define (set-contains? s element) (set-member? s element))
(define (set-contains-all? s elements) (subset? (sequence->set elements) s))
(define (set-contains-any? s elements) (not (set-contains-none? s elements)))

(define (set-contains-none? s elements)
  (equal? (set-count (set-remove-all s elements)) (set-count s)))

(define (set-add-all s elements) (set-union s (sequence->set elements)))
(define (set-remove-all s elements) (set-subtract s (sequence->set elements)))

(module+ test
  (test-case "set-add-all"
    (check-equal? (set-add-all (set 1 2) (set 3 4 5)) (set 1 2 3 4 5))
    (check-equal? (set-add-all (set 1 2) (list 3 4 5)) (set 1 2 3 4 5))
    (check-equal? (set-add-all (set 1 2) (list 3 4 4 5)) (set 1 2 3 4 5)))

  (test-case "set-remove-all"
    (check-equal? (set-remove-all (set 1 2 3 4) (set 1 3)) (set 2 4))
    (check-equal? (set-remove-all (set 1 2 3 4) (list 1 3)) (set 2 4))
    (check-equal? (set-remove-all (set 1 2 3 4) (list 1 3 3)) (set 2 4))))

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

(define-simple-macro (define-external-pict name:id source:expr)
  (begin
    (define-runtime-path external-pict-path source)
    (define name (bitmap external-pict-path))))

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
