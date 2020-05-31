#lang racket/base

(require racket/contract/base)

(provide
 (contract-out
  [set-act
   (->i #:chaperone
        ([set set?] [action set-action?])
        #:pre/name (set action)
        "The action must be applicable to the set."
        (set-action-applicable? action set)
        [_ set?])]
  [set-action
   (->* ()
        (#:requirements set-coercible-sequence/c
         #:obstructions set-coercible-sequence/c
         #:additions set-coercible-sequence/c
         #:deletions set-coercible-sequence/c
         #:cost (>=/c 0))
        set-action?)]
  [set-action? predicate/c]
  [set-action-applicable? (-> set-action? set? boolean?)]
  [set-action-requirements (-> set-action? set?)]
  [set-action-obstructions (-> set-action? set?)]
  [set-action-additions (-> set-action? set?)]
  [set-action-deletions (-> set-action? set?)]
  [set-action-cost (-> set-action? (>=/c 0))]
  [set-action-preconditions (-> set-action? set-condition?)]))

(require fancy-app
         planning/private
         planning/set/condition
         point-free
         racket/set
         rebellion/collection/set
         rebellion/type/record)

(module+ test
  (require (submod "..")
           rackunit))

;@------------------------------------------------------------------------------

(define-record-type set-action
  (requirements obstructions additions deletions cost)
  #:omit-root-binding)

(define (set-action #:requirements [requirements empty-set]
                    #:obstructions [obstructions empty-set]
                    #:additions [additions empty-set]
                    #:deletions [deletions empty-set]
                    #:cost [cost 1])
  (constructor:set-action
   #:requirements (sequence->set requirements)
   #:obstructions (sequence->set obstructions)
   #:additions (sequence->set additions)
   #:deletions (sequence->set deletions)
   #:cost cost))

(define (set-action-preconditions action)
  (set-condition
   #:requirements (set-action-requirements action)
   #:obstructions (set-action-obstructions action)))

(define (set-action-applicable? action set)
  (set-meets-condition? set (set-action-preconditions action)))

(define (set-act set action)
  (~> (set-add-all set (set-action-additions action))
      (set-remove-all _ (set-action-deletions action))))

(module+ test
  (test-case "set-act"
    (test-case "additions"
      (define s (set 1 2 3))
      (define action (set-action #:additions (set 4 5)))
      (check-equal? (set-act s action) (set 1 2 3 4 5)))

    (test-case "redundant additions"
      (define s (set 1 2 3))
      (define action (set-action #:additions (set 1 2)))
      (check-equal? (set-act s action) s))

    (test-case "deletions"
      (define s (set 1 2 3))
      (define action (set-action #:deletions (set 2 3)))
      (check-equal? (set-act s action) (set 1)))

    (test-case "redundant deletions"
      (define s (set 1 2 3))
      (define action (set-action #:deletions (set 4 5)))
      (check-equal? (set-act s action) s))))
