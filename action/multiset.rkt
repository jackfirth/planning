#lang racket/base

(require racket/contract/base)

(provide
 (contract-out
  [multiset-action
   (->i ()
        (#:additions [additions multiset?]
         #:deletions [deletions multiset?]
         #:replacements [replacements (hash/c any/c exact-nonnegative-integer?)]
         #:requirements [requirements (hash/c any/c range?)])
        [_ multiset-action?])]
  [multiset-action? predicate/c]
  [multiset-action-additions (-> multiset-action? multiset?)]
  [multiset-action-deletions (-> multiset-action? multiset?)]
  [multiset-action-replacements
   (-> multiset-action? (hash/c any/c exact-nonnegative-integer?))]
  [multiset-action-requirements
   (-> multiset-action? (hash/c any/c range?))]
  [multiset-action-applicable? (-> multiset-action? multiset? boolean?)]
  [multiset-action-perform (-> multiset-action? multiset? multiset?)]))

(require fancy-app
         planning/private
         point-free
         rebellion/base/range
         rebellion/collection/hash
         rebellion/collection/multiset
         rebellion/type/record)

;@------------------------------------------------------------------------------

(define-record-type multiset-action
  (additions deletions replacements requirements)
  #:omit-root-binding)

(define (multiset-action #:additions [additions empty-multiset]
                         #:deletions [deletions empty-multiset]
                         #:replacements [replacements empty-hash]
                         #:requirements [requirements empty-hash])
  (constructor:multiset-action
   #:additions additions
   #:deletions deletions
   #:replacements replacements
   #:requirements requirements))

(define (multiset-action-applicable? action state-multiset)
  (define requirements (multiset-action-requirements action))
  (for/and ([(element acceptable-frequencies) (in-immutable-hash requirements)])
    (define freq (multiset-frequency state-multiset element))
    (range-contains? acceptable-frequencies freq)))

(define (multiset-action-perform action state-multiset)
  (define deletions (multiset-action-deletions action))
  (define additions (multiset-action-additions action))
  (define replacements (multiset-action-replacements action))
  (~> (multiset-remove-all state-multiset deletions)
      (multiset-add-all _ additions)
      (multiset-set-all-frequencies _ replacements)))
