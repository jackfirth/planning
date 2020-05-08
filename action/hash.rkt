#lang racket/base

(require racket/contract/base)

(provide
 (contract-out
  [hash-action
   (->i ()
        (#:requirements [requirements multidict?]
         #:required-keys [required-keys set?]
         #:obstructors [obstructors multidict?]
         #:obstructing-keys [obstructing-keys set?]
         #:additions [additions immutable-hash?]
         #:deletions [deletions set?])
        [_ hash-action?])]
  [hash-action? predicate/c]
  [hash-action-requirements (-> hash-action? multidict?)]
  [hash-action-required-keys (-> hash-action? set?)]
  [hash-action-obstructors (-> hash-action? multidict?)]
  [hash-action-obstructing-keys (-> hash-action? multidict?)]
  [hash-action-additions (-> hash-action? immutable-hash?)]
  [hash-action-deletions (-> hash-action? set?)]
  [hash-action-perform (-> hash-action? immutable-hash? immutable-hash?)]
  [hash-action-applicable? (-> hash-action? immutable-hash? boolean?)]))

(require planning/private
         racket/set
         rebellion/collection/hash
         rebellion/collection/multidict
         rebellion/collection/set
         rebellion/type/record)

;@------------------------------------------------------------------------------

(define-record-type hash-action
  (requirements obstructors required-keys obstructing-keys additions deletions)
  #:omit-root-binding)

(define (hash-action #:requirements [requirements empty-multidict]
                     #:required-keys [required-keys empty-set]
                     #:obstructors [obstructors empty-multidict]
                     #:obstructing-keys [obstructing-keys empty-set]
                     #:additions [additions empty-hash]
                     #:deletions [deletions empty-set])
  (constructor:hash-action
   #:requirements requirements
   #:required-keys required-keys
   #:obstructors obstructors
   #:obstructing-keys obstructing-keys
   #:additions additions
   #:deletions deletions))

(define (hash-action-applicable? action state-hash)
  (define keys (hash-key-set state-hash))
  (define requirements (hash-action-requirements action))
  (and (set-contains-all? keys (hash-action-required-keys action))
       (set-contains-none? keys (hash-action-obstructing-keys action))
       (for/and ([k (in-immutable-set (multidict-unique-keys requirements))])
         (and (hash-has-key? state-hash k)
              (set-contains? (multidict-ref requirements k)
                             (hash-ref state-hash k))))
       (hash-contains-none? state-hash (hash-action-obstructors action))))

(define (hash-action-perform action state-hash)
  (hash-put-all (hash-remove-all state-hash (hash-action-deletions action))
                (hash-action-additions action)))
