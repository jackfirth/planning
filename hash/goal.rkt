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

(module+ test
  (require (submod "..")
           rackunit))

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
  (define values (sequence->set (hash-values hash)))
  (define requirements (hash-goal-requirements goal))
  (and (set-contains-all? keys (hash-goal-required-keys goal))
       (set-contains-all? values (hash-goal-required-values goal))
       (set-contains-none? keys (hash-goal-obstructing-keys goal))
       (set-contains-none? values (hash-goal-obstructing-values goal))
       (for/and ([k (in-immutable-set (multidict-unique-keys requirements))])
         (and (hash-has-key? hash k)
              (set-contains? (multidict-ref requirements k)
                             (hash-ref hash k))))
       (hash-contains-none? hash (hash-goal-obstructions goal))))

(module+ test
  (test-case "hash-goal-achieved?"
    (test-case "requirements"
      (define goal (hash-goal #:requirements (multidict 'a 1 'a 2 'b 3)))
      (check-true (hash-goal-achieved? goal (hash 'a 1 'b 3 'c 4)))
      (check-true (hash-goal-achieved? goal (hash 'a 2 'b 3 'c 4)))
      (check-false (hash-goal-achieved? goal (hash 'a 3 'b 3 'c 4)))
      (check-false (hash-goal-achieved? goal (hash 'a 1 'b 1)))
      (check-false (hash-goal-achieved? goal (hash 'a 1)))
      (check-false (hash-goal-achieved? goal (hash 'b 3))))

    (test-case "required-keys"
      (define goal (hash-goal #:required-keys (set 'a 'b)))
      (check-true (hash-goal-achieved? goal (hash 'a 1 'b 2)))
      (check-true (hash-goal-achieved? goal (hash 'a 1 'b 2 'c 3)))
      (check-false (hash-goal-achieved? goal (hash 'a 1)))
      (check-false (hash-goal-achieved? goal (hash 'b 2))))

    (test-case "required-values"
      (define goal (hash-goal #:required-values (set 1 2)))
      (check-true (hash-goal-achieved? goal (hash 'a 1 'b 2)))
      (check-true (hash-goal-achieved? goal (hash 'a 1 'b 2 'c 3)))
      (check-true (hash-goal-achieved? goal (hash 'a 2 'b 1)))
      (check-false (hash-goal-achieved? goal (hash 'a 1 'b 1)))
      (check-false (hash-goal-achieved? goal (hash 'a 2 'b 2))))))
