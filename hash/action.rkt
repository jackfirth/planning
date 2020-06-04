#lang racket/base

(require racket/contract/base)

(provide
 (contract-out
  [hash-act (-> immutable-hash? hash-action? immutable-hash?)]
  [hash-action
   (->i ()
        (#:requirements [requirements multidict?]
         #:required-keys [required-keys set?]
         #:required-values [required-values set?]
         #:obstructions [obstructions multidict?]
         #:obstructing-keys [obstructing-keys set?]
         #:obstructing-values [obstructing-values set?]
         #:additions [additions immutable-hash?]
         #:deletions [deletions set?])
        [_ hash-action?])]
  [hash-action? predicate/c]
  [hash-action-requirements (-> hash-action? multidict?)]
  [hash-action-required-keys (-> hash-action? set?)]
  [hash-action-required-values (-> hash-action? set?)]
  [hash-action-obstructions (-> hash-action? multidict?)]
  [hash-action-obstructing-keys (-> hash-action? set?)]
  [hash-action-obstructing-values (-> hash-action? set?)]
  [hash-action-additions (-> hash-action? immutable-hash?)]
  [hash-action-deletions (-> hash-action? set?)]
  [hash-action-applicable? (-> hash-action? immutable-hash? boolean?)]))

(require planning/hash/condition
         planning/private
         racket/set
         rebellion/collection/hash
         rebellion/collection/multidict
         rebellion/collection/set
         rebellion/type/record)

;@------------------------------------------------------------------------------

(define-record-type hash-action
  (requirements
   required-keys
   required-values
   obstructions
   obstructing-keys
   obstructing-values
   additions
   deletions)
  #:omit-root-binding)

(define (hash-action #:requirements [requirements empty-multidict]
                     #:required-keys [required-keys empty-set]
                     #:required-values [required-values empty-set]
                     #:obstructions [obstructions empty-multidict]
                     #:obstructing-keys [obstructing-keys empty-set]
                     #:obstructing-values [obstructing-values empty-set]
                     #:additions [additions empty-hash]
                     #:deletions [deletions empty-set])
  (constructor:hash-action
   #:requirements requirements
   #:required-keys required-keys
   #:required-values required-values
   #:obstructions obstructions
   #:obstructing-keys obstructing-keys
   #:obstructing-values obstructing-values
   #:additions additions
   #:deletions deletions))

(define (hash-action-applicable? action state-hash)
  (define preconditions
    (hash-condition
     #:requirements (hash-action-requirements action)
     #:required-keys (hash-action-required-keys action)
     #:required-values (hash-action-required-values action)
     #:obstructions (hash-action-obstructions action)
     #:obstructing-keys (hash-action-obstructing-keys action)
     #:obstructing-values (hash-action-obstructing-values action)))
  (hash-meets-condition? state-hash preconditions))

(define (hash-act state-hash action)
  (hash-put-all (hash-remove-all state-hash (hash-action-deletions action))
                (hash-action-additions action)))
