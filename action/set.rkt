#lang racket/base

(require racket/contract/base)

(provide
 (contract-out
  [set-action
   (->i ()
        (#:requirements [requirements set?]
         #:obstructors [obstructors set?]
         #:additions [additions set?]
         #:deletions [deletions set?])

        #:pre/name (requirements obstructors)
        "cannot both require and be obstructed by the same fluent"
        (set-contains-none? (default requirements empty-set)
                            (default obstructors empty-set))

        #:pre/name (additions deletions)
        "cannot both add and delete the same fluent"
        (set-contains-none? (default additions empty-set)
                            (default deletions empty-set))

        #:pre/name (requirements additions)
        "redundantly adding a required fluent is not allowed"
        (set-contains-none? (default requirements empty-set)
                            (default additions empty-set))

        #:pre/name (obstructors deletions)
        "redundantly deleting an obstructing fluent is not allowed"
        (set-contains-none? (default obstructors empty-set)
                            (default deletions empty-set))
        
        [_ set-action?])]
  [set-action? predicate/c]
  [set-action-requirements (-> set-action? set?)]
  [set-action-obstructors (-> set-action? set?)]
  [set-action-additions (-> set-action? set?)]
  [set-action-deletions (-> set-action? set?)]
  [set-action-preconditions
   (-> set-action? (hash/c any/c set-precondition-fluent-type? #:immutable #t))]
  [set-action-effects
   (-> set-action? (hash/c any/c set-effect-fluent-type? #:immutable #t))]
  [set-action-applicable? (-> set-action? set? boolean?)]
  [set-action-perform
   (->i ([action set-action?] [state-fluents set?])
        #:pre/name (action state-fluents)
        "action must be applicable in given state"
        (set-action-applicable? action state-fluents)
        [_ set?])]
  [set-precondition-fluent-type? predicate/c]
  [required-fluent set-precondition-fluent-type?]
  [obstructing-fluent set-precondition-fluent-type?]
  [set-effect-fluent-type? predicate/c]
  [added-fluent set-effect-fluent-type?]
  [deleted-fluent set-effect-fluent-type?]
  [set-action-mutually-excludes? (-> set-action? set-action? boolean?)]))

(require planning/private
         racket/set
         rebellion/collection/entry
         rebellion/collection/hash
         rebellion/collection/list
         rebellion/collection/set
         rebellion/streaming/transducer
         rebellion/type/enum
         rebellion/type/record)

;@------------------------------------------------------------------------------

(define-record-type set-action
  (requirements obstructors additions deletions)
  #:omit-root-binding)

(define (set-action #:requirements [requirements empty-set]
                    #:obstructors [obstructors empty-set]
                    #:additions [additions empty-set]
                    #:deletions [deletions empty-set])
  (constructor:set-action #:requirements requirements
                          #:obstructors obstructors
                          #:additions additions
                          #:deletions deletions))

(define-enum-type set-precondition-fluent-type
  (required-fluent obstructing-fluent))

(define-enum-type set-effect-fluent-type (added-fluent deleted-fluent))

(define (set-action-preconditions action)
  (define requirements
    (transduce (set-action-requirements action)
               (bisecting values (λ (_) required-fluent))
               #:into into-list))
  (define obstructors
    (transduce (set-action-obstructors action)
               (bisecting values (λ (_) obstructing-fluent))
               #:into into-list))
  (transduce (append requirements obstructors) #:into into-hash))

(define (set-action-effects action)
  (define additions
    (transduce (set-action-additions action)
               (bisecting values (λ (_) added-fluent))
               #:into into-list))
  (define deletions
    (transduce (set-action-deletions action)
               (bisecting values (λ (_) deleted-fluent))
               #:into into-list))
  (transduce (append additions deletions) #:into into-hash))

(define (set-action-applicable? action state-fluents)
  (and (set-contains-all? (set-action-requirements action) state-fluents)
       (set-contains-none? (set-action-obstructors action) state-fluents)))

(define (set-action-perform action state-fluents)
  (define additions (set-action-additions action))
  (define deletions (set-action-deletions action))
  (set-subtract (set-union additions state-fluents) deletions))

(define (action-effects-inconsistent? act1 act2)
  (or (set-contains-any? (set-action-deletions act1)
                         (set-action-additions act2))
      (set-contains-any? (set-action-additions act1)
                         (set-action-deletions act2))))

(define (action-interferes? act1 act2)
  (or (set-contains-any? (set-action-additions act1)
                         (set-action-obstructors act2))
      (set-contains-any? (set-action-deletions act1)
                         (set-action-requirements act2))
      (set-contains-any? (set-action-additions act2)
                         (set-action-obstructors act1))
      (set-contains-any? (set-action-deletions act2)
                         (set-action-requirements act1))))

(define (action-has-competing-needs? act1 act2)
  (or (set-contains-any? (set-action-requirements act1)
                         (set-action-obstructors act2))
      (set-contains-any? (set-action-obstructors act1)
                         (set-action-requirements act2))))

(define (set-action-mutually-excludes? act1 act2)
  (or (action-effects-inconsistent? act1 act2)
      (action-interferes? act1 act2)
      (action-has-competing-needs? act1 act2)))
