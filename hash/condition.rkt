#lang racket/base

(require racket/contract/base)

(provide
 (contract-out
  [hash-condition? predicate/c]
  [hash-condition
   (->i ()
        (#:requirements [requirements multidict?]
         #:required-keys [required-keys set?]
         #:required-values [required-values set?]
         #:obstructions [obstructions multidict?]
         #:obstructing-keys [obstructing-keys set?]
         #:obstructing-values [obstructing-values set?])
        [_ hash-condition?])]
  [hash-meets-condition? (-> hash? hash-condition? boolean?)]
  [hash-condition-requirements (-> hash-condition? multidict?)]
  [hash-condition-required-keys (-> hash-condition? set?)]
  [hash-condition-required-values (-> hash-condition? set?)]
  [hash-condition-obstructions (-> hash-condition? multidict?)]
  [hash-condition-obstructing-keys (-> hash-condition? set?)]
  [hash-condition-obstructing-values (-> hash-condition? set?)]))

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

(define-record-type hash-condition
  (requirements
   required-keys
   required-values
   obstructions
   obstructing-keys
   obstructing-values)
  #:omit-root-binding)

(define (hash-condition #:requirements [requirements empty-multidict]
                        #:required-keys [required-keys empty-set]
                        #:required-values [required-values empty-set]
                        #:obstructions [obstructions empty-multidict]
                        #:obstructing-keys [obstructing-keys empty-set]
                        #:obstructing-values [obstructing-values empty-set])
  (constructor:hash-condition
   #:requirements requirements
   #:required-keys required-keys
   #:required-values required-values
   #:obstructions obstructions
   #:obstructing-keys obstructing-keys
   #:obstructing-values obstructing-values))

(define (hash-meets-condition? hash goal)
  (define keys (hash-key-set hash))
  (define values (sequence->set (hash-values hash)))
  (define requirements (hash-condition-requirements goal))
  (and (set-contains-all? keys (hash-condition-required-keys goal))
       (set-contains-all? values (hash-condition-required-values goal))
       (set-contains-none? keys (hash-condition-obstructing-keys goal))
       (set-contains-none? values (hash-condition-obstructing-values goal))
       (for/and ([k (in-immutable-set (multidict-unique-keys requirements))])
         (and (hash-has-key? hash k)
              (set-contains? (multidict-ref requirements k)
                             (hash-ref hash k))))
       (hash-contains-none? hash (hash-condition-obstructions goal))))

(module+ test
  (test-case "hash-goal-achieved?"
    (test-case "requirements"
      (define condition
        (hash-condition #:requirements (multidict 'a 1 'a 2 'b 3)))
      (check-true (hash-meets-condition?  (hash 'a 1 'b 3 'c 4) condition))
      (check-true (hash-meets-condition? (hash 'a 2 'b 3 'c 4) condition))
      (check-false (hash-meets-condition? (hash 'a 3 'b 3 'c 4) condition))
      (check-false (hash-meets-condition? (hash 'a 1 'b 1) condition))
      (check-false (hash-meets-condition? (hash 'a 1) condition))
      (check-false (hash-meets-condition? (hash 'b 3) condition)))

    (test-case "required-keys"
      (define condition (hash-condition #:required-keys (set 'a 'b)))
      (check-true (hash-meets-condition? (hash 'a 1 'b 2) condition))
      (check-true (hash-meets-condition? (hash 'a 1 'b 2 'c 3) condition))
      (check-false (hash-meets-condition? (hash 'a 1) condition))
      (check-false (hash-meets-condition? (hash 'b 2) condition)))

    (test-case "required-values"
      (define condition (hash-condition #:required-values (set 1 2)))
      (check-true (hash-meets-condition? (hash 'a 1 'b 2) condition))
      (check-true (hash-meets-condition? (hash 'a 1 'b 2 'c 3) condition))
      (check-true (hash-meets-condition? (hash 'a 2 'b 1) condition))
      (check-false (hash-meets-condition? (hash 'a 1 'b 1) condition))
      (check-false (hash-meets-condition? (hash 'a 2 'b 2) condition)))))
