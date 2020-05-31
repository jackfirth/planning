#lang racket/base

(require racket/contract/base)

(provide
 (contract-out
  [multiset-goal? predicate/c]
  [multiset-goal
   (-> (hash/c any/c nonempty-range? #:immutable #t) multiset-goal?)]
  [multiset-goal-requirements
   (-> multiset-goal? (hash/c any/c nonempty-range? #:immutable #t))]
  [multiset-goal-achieved? (-> multiset-goal? multiset? boolean?)]
  [multiset-goal-ignore-frequencies (-> multiset-goal? set-condition?)]))

(require planning/set/condition
         racket/set
         rebellion/base/range
         rebellion/collection/multiset
         rebellion/type/tuple)

;@------------------------------------------------------------------------------

(define-tuple-type multiset-goal (requirements))

(define (multiset-goal-achieved? goal set)
  (define requirements (multiset-goal-requirements goal))
  (for/and ([(element frequency-range) (in-immutable-hash requirements)])
    (range-contains? frequency-range (multiset-frequency set element))))

(define (multiset-goal-ignore-frequencies goal)
  (define requirements (multiset-goal-requirements goal))
  (define required-elements
    (for/set ([(element frequency-range) (in-immutable-hash requirements)]
              #:unless (range-contains? frequency-range 0))
      element))
  (define obstructions
    (for/set ([(element frequency-range) (in-immutable-hash requirements)]
              #:when (range-encloses? (singleton-range 0) frequency-range))
      element))
  (set-condition #:requirements required-elements #:obstructions obstructions))
