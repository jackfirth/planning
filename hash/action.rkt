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

(require planning/private
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
  (define keys (hash-key-set state-hash))
  (define requirements (hash-action-requirements action))
  (and (set-contains-all? keys (hash-action-required-keys action))
       (set-contains-none? keys (hash-action-obstructing-keys action))
       (for/and ([k (in-immutable-set (multidict-unique-keys requirements))])
         (and (hash-has-key? state-hash k)
              (set-contains? (multidict-ref requirements k)
                             (hash-ref state-hash k))))
       (hash-contains-none? state-hash (hash-action-obstructions action))))

(define (hash-act state-hash action)
  (hash-put-all (hash-remove-all state-hash (hash-action-deletions action))
                (hash-action-additions action)))
