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
  [set-action-preconditions (-> set-action? set-condition?)]
  [set-action-postconditions (-> set-action? set-condition?)]))

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

(define (set-action-postconditions action)
  (define post-requirements
    (~> (set-action-requirements action)
        (set-add-all _ (set-action-additions action))
        (set-remove-all _ (set-action-deletions action))))
  (define post-obstructions
    (~> (set-action-obstructions action)
        (set-remove-all _ (set-action-additions action))
        (set-add-all _ (set-action-deletions action))))
  (set-condition
   #:requirements post-requirements #:obstructions post-obstructions))

(module+ test
  (test-case "set-action-postconditions"
    (test-case "actions with no effects persist preconditions"
      (define action
        (set-action #:requirements (set 1 2 3) #:obstructions (set 4 5)))
      (check-equal? (set-action-postconditions action)
                    (set-action-preconditions action)))

    (test-case "an action's additions must be present after it's applied"
      (define action (set-action #:additions (set 1 2 3)))
      (check-equal? (set-action-postconditions action)
                    (set-condition #:requirements (set 1 2 3))))

    (test-case "an action's deletions must be absent after it's applied"
      (define action (set-action #:deletions (set 1 2 3)))
      (check-equal? (set-action-postconditions action)
                    (set-condition #:obstructions (set 1 2 3))))

    (test-case "an action's obstructions are invalidated by its additions"
      (define action
        (set-action #:obstructions (set 1 2 3 4 5) #:additions (set 1 2)))
      (define expected
        (set-condition #:requirements (set 1 2) #:obstructions (set 3 4 5)))
      (check-equal? (set-action-postconditions action) expected))

    (test-case "an action's requirements are invalidated by its deletions"
      (define action
        (set-action #:requirements (set 1 2 3 4 5) #:deletions (set 1 2)))
      (define expected
        (set-condition #:requirements (set 3 4 5) #:obstructions (set 1 2)))
      (check-equal? (set-action-postconditions action) expected))))
