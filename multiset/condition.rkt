#lang racket/base

(require racket/contract/base)

(provide
 (contract-out
  [multiset-meets-condition? (-> multiset? multiset-condition? boolean?)]
  [multiset-condition? predicate/c]
  [multiset-condition
   (-> (hash/c any/c nonempty-range? #:immutable #t) multiset-condition?)]
  [multiset-condition-requirements
   (-> multiset-condition? (hash/c any/c nonempty-range? #:immutable #t))]
  [multiset-condition-ignore-frequencies
   (-> multiset-condition? set-condition?)]))

(require planning/set/condition
         racket/set
         rebellion/base/range
         rebellion/collection/multiset
         rebellion/type/tuple)

;@------------------------------------------------------------------------------

(define-tuple-type multiset-condition (requirements))

(define (multiset-meets-condition? set goal)
  (define requirements (multiset-condition-requirements goal))
  (for/and ([(element frequency-range) (in-immutable-hash requirements)])
    (range-contains? frequency-range (multiset-frequency set element))))

(define (multiset-condition-ignore-frequencies goal)
  (define requirements (multiset-condition-requirements goal))
  (define required-elements
    (for/set ([(element frequency-range) (in-immutable-hash requirements)]
              #:unless (range-contains? frequency-range 0))
      element))
  (define obstructions
    (for/set ([(element frequency-range) (in-immutable-hash requirements)]
              #:when (range-encloses? (singleton-range 0) frequency-range))
      element))
  (set-condition #:requirements required-elements #:obstructions obstructions))
