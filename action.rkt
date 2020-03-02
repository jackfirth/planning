#lang racket/base

(require racket/contract/base)

(provide
 (contract-out
  [action
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
        
        [_ action?])]
  [action? predicate/c]
  [action-requirements (-> action? set?)]
  [action-obstructors (-> action? set?)]
  [action-additions (-> action? set?)]
  [action-deletions (-> action? set?)]
  [action-preconditions (-> action? (hash/c any/c precondition-fluent-type?))]
  [action-effects (-> action? (hash/c any/c effect-fluent-type?))]
  [action-applicable? (-> action? set? boolean?)]
  [action-execute
   (->i ([action action?] [state-fluents set?])
        #:pre/name (action state-fluents)
        "action must be applicable in given state"
        (action-applicable? action state-fluents)
        [_ set?])]
  [precondition-fluent-type? predicate/c]
  [required-fluent precondition-fluent-type?]
  [obstructing-fluent precondition-fluent-type?]
  [effect-fluent-type? predicate/c]
  [added-fluent effect-fluent-type?]
  [deleted-fluent effect-fluent-type?]))

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

(define-record-type action
  (requirements obstructors additions deletions)
  #:omit-root-binding)

(define (action #:requirements [requirements empty-set]
                #:obstructors [obstructors empty-set]
                #:additions [additions empty-set]
                #:deletions [deletions empty-set])
  (constructor:action #:requirements requirements
                      #:obstructors obstructors
                      #:additions additions
                      #:deletions deletions))

(define-enum-type precondition-fluent-type (required-fluent obstructing-fluent))
(define-enum-type effect-fluent-type (added-fluent deleted-fluent))

(define (action-preconditions action)
  (define requirements
    (transduce (action-requirements action)
               (bisecting values (λ (_) required-fluent))
               #:into into-list))
  (define obstructors
    (transduce (action-obstructors action)
               (bisecting values (λ (_) obstructing-fluent))
               #:into into-list))
  (transduce (append requirements obstructors) #:into into-hash))

(define (action-effects action)
  (define additions
    (transduce (action-additions action)
               (bisecting values (λ (_) added-fluent))
               #:into into-list))
  (define deletions
    (transduce (action-deletions action)
               (bisecting values (λ (_) deleted-fluent))
               #:into into-list))
  (transduce (append additions deletions) #:into into-hash))

(define (action-applicable? action state-fluents)
  (and (set-contains-all? (action-requirements action) state-fluents)
       (set-contains-none? (action-obstructors action) state-fluents)))

(define (action-execute action state-fluents)
  (define additions (action-additions action))
  (define deletions (action-deletions action))
  (set-subtract (set-union additions state-fluents) deletions))

(define (default arg default-value)
  (if (unsupplied-arg? arg) default-value arg))
