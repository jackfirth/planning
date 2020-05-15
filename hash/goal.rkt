#lang racket/base

(require racket/contract/base)

(provide
 (contract-out
  [hash-goal? predicate/c]
  [hash-goal
   (->i ()
        (#:requirements [requirements multidict?]
         #:required-keys [required-keys set?]
         #:required-values [required-values set?]
         #:obstructions [obstructions multidict?]
         #:obstructing-keys [obstructing-keys set?]
         #:obstructing-values [obstructing-values set?])
        [_ hash-goal?])]
  [hash-goal-achieved? (-> hash-goal? hash? boolean?)]
  [hash-goal-requirements (-> hash-goal? multidict?)]
  [hash-goal-required-keys (-> hash-goal? set?)]
  [hash-goal-required-values (-> hash-goal? set?)]
  [hash-goal-obstructions (-> hash-goal? multidict?)]
  [hash-goal-obstructing-keys (-> hash-goal? set?)]
  [hash-goal-obstructing-values (-> hash-goal? set?)]))

(require planning/private
         racket/set
         rebellion/collection/hash
         rebellion/collection/multidict
         rebellion/collection/set
         rebellion/type/record)

;@------------------------------------------------------------------------------

(define-record-type hash-goal
  (requirements
   required-keys
   required-values
   obstructions
   obstructing-keys
   obstructing-values)
  #:omit-root-binding)

(define (hash-goal #:requirements [requirements empty-multidict]
                   #:required-keys [required-keys empty-set]
                   #:required-values [required-values empty-set]
                   #:obstructions [obstructions empty-multidict]
                   #:obstructing-keys [obstructing-keys empty-set]
                   #:obstructing-values [obstructing-values empty-set])
  (constructor:hash-goal #:requirements requirements
                         #:required-keys required-keys
                         #:required-values required-values
                         #:obstructions obstructions
                         #:obstructing-keys obstructing-keys
                         #:obstructing-values obstructing-values))

(define (hash-goal-achieved? goal hash)
  (define keys (hash-key-set hash))
  (define requirements (hash-goal-requirements goal))
  (and (set-contains-all? keys (hash-goal-required-keys goal))
       (set-contains-none? keys (hash-goal-obstructing-keys goal))
       (for/and ([k (in-immutable-set (multidict-unique-keys requirements))])
         (and (hash-has-key? hash k)
              (set-contains? (multidict-ref requirements k)
                             (hash-ref hash k))))
       (hash-contains-none? hash (hash-goal-obstructions goal))))
