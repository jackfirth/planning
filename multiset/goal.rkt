#lang racket/base

(require racket/contract/base)

(provide
 (contract-out
  [multiset-goal? predicate/c]
  [multiset-goal (-> (hash/c any/c range? #:immutable #t) multiset-goal?)]
  [multiset-goal-requirements
   (-> multiset-goal? (hash/c any/c range? #:immutable #t))]
  [multiset-goal-achieved? (-> multiset-goal? multiset? boolean?)]))

(require rebellion/base/range
         rebellion/collection/multiset
         rebellion/type/tuple)

;@------------------------------------------------------------------------------

(define-tuple-type multiset-goal (requirements))

(define (multiset-goal-achieved? goal set)
  (define requirements (multiset-goal-requirements goal))
  (for/and ([(element frequency-range) (in-immutable-hash requirements)])
    (range-contains? frequency-range (multiset-frequency set element))))
