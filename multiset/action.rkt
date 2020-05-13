#lang racket/base

(require racket/contract/base)

(provide
 (contract-out
  [multiset-action
   (->i ()
        (#:preconditions [preconditions (hash/c any/c range? #:immutable #t)]
         #:deletions [deletions multiset?]
         #:additions [additions multiset?]
         #:replacements [replacements (hash/c any/c natural? #:immutable #t)]
         #:cost [cost (>=/c 0)])
        [_ multiset-action?])]
  [multiset-action? predicate/c]
  [multiset-action-preconditions
   (-> multiset-action? (hash/c any/c range? #:immutable #t))]
  [multiset-action-deletions (-> multiset-action? multiset?)]
  [multiset-action-additions (-> multiset-action? multiset?)]
  [multiset-action-replacements
   (-> multiset-action? (hash/c any/c natural? #:immutable #t))]
  [multiset-action-cost (-> multiset-action? (>=/c 0))]
  [multiset-action-applicable? (-> multiset-action? multiset? boolean?)]
  [multiset-action-perform (-> multiset-action? multiset? multiset?)]))

(require fancy-app
         planning/private
         point-free
         racket/math
         rebellion/base/range
         rebellion/collection/hash
         rebellion/collection/multiset
         rebellion/type/record)

;@------------------------------------------------------------------------------

(define-record-type multiset-action
  (additions deletions replacements preconditions cost)
  #:omit-root-binding)

(define (multiset-action #:preconditions [preconditions empty-hash]
                         #:deletions [deletions empty-multiset]
                         #:additions [additions empty-multiset]
                         #:replacements [replacements empty-hash]
                         #:cost [cost 1])
  (constructor:multiset-action
   #:preconditions preconditions
   #:deletions deletions
   #:additions additions
   #:replacements replacements
   #:cost 1))

(define (multiset-action-applicable? action state-multiset)
  (for/and ([(element acceptable-frequencies)
             (in-immutable-hash (multiset-action-preconditions action))])
    (define freq (multiset-frequency state-multiset element))
    (range-contains? acceptable-frequencies freq)))

(define (multiset-action-perform action state-multiset)
  (define deletions (multiset-action-deletions action))
  (define additions (multiset-action-additions action))
  (define replacements (multiset-action-replacements action))
  (~> (multiset-remove-all state-multiset deletions)
      (multiset-add-all _ additions)
      (multiset-set-all-frequencies _ replacements)))
