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
        (set-contains-none? requirements obstructors)

        #:pre/name (additions deletions)
        "cannot both add and delete the same fluent"
        (set-contains-none? additions deletions)

        #:pre/name (requirements additions)
        "redundantly adding a required fluent is not allowed"
        (set-contains-none? requirements additions)

        #:pre/name (obstructors deletions)
        "redundantly deleting an obstructing fluent is not allowed"
        (set-contains-none? obstructors deletions)
        
        [_ action?])]
  [action? predicate/c]
  [action-requirements (-> action? set?)]
  [action-obstructors (-> action? set?)]
  [action-additions (-> action? set?)]
  [action-deletions (-> action? set?)]
  [action-preconditions (-> action? (hash/c any/c action-precondition-type?))]
  [action-effects (-> action? (hash/c any/c action-effect-type?))]
  [action-applicable? (-> action? set? boolean?)]
  [action-execute
   (->i ([action action?] [state-fluents set?])
        #:pre/name (action state-fluents)
        "action must be applicable in given state"
        (action-applicable? action state-fluents)
        [_ set?])]
  [action-precondition-type? predicate/c]
  [required-fluent action-precondition-type?]
  [obstructing-fluent action-precondition-type?]
  [action-effect-type? predicate/c]
  [added-fluent action-effect-type?]
  [deleted-fluent action-effect-type?]))

(require planning/private
         racket/set
         rebellion/collection/entry
         rebellion/collection/hash
         rebellion/collection/list
         rebellion/collection/set
         rebellion/streaming/transducer
         rebellion/type/enum
         rebellion/type/record
         rebellion/type/tuple)

(module+ test
  (require (submod "..")
           rackunit))

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

(define-enum-type action-precondition-type (required-fluent obstructing-fluent))
(define-enum-type action-effect-type (added-fluent deleted-fluent))

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

;@------------------------------------------------------------------------------
;; The Room World contains two rooms, a door between them, and a player object
;; who resides in one of the rooms. The player may move between rooms if the
;; door is open, as well as open and close the door. This simple world is mainly
;; useful for planning demonstrations and testing.

(define-enum-type room-world-object
  (player left-room right-room door))

(define-tuple-type is-player (claimed-player))
(define-tuple-type is-room (claimed-room))
(define-tuple-type is-door (claimed-door))
(define-tuple-type at (subject stated-position))
(define-tuple-type is-empty (subject-room))
(define-tuple-type is-closed (subject-door))

(define move-left
  (action #:requirements
          (set (is-player player)
               (is-room left-room)
               (is-room right-room)
               (is-door door)
               (at player right-room)
               (is-empty left-room))
          #:obstructors
          (set (is-closed door))
          #:additions
          (set (at player left-room)
               (is-empty right-room))
          #:deletions
          (set (at player right-room)
               (is-empty left-room))))

(define move-right
  (action
   #:requirements
   (set (is-player player)
        (is-room left-room)
        (is-room right-room)
        (is-door door)
        (at player left-room)
        (is-empty right-room))
   #:obstructors
   (set (is-closed door))
   #:additions
   (set (at player right-room)
        (is-empty left-room))
   #:deletions
   (set (at player left-room)
        (is-empty right-room))))

(define open-door
  (action
   #:requirements
   (set (is-player player)
        (is-door door)
        (is-closed door))
   #:deletions
   (set (is-closed door))))

(define close-door
  (action
   #:requirements
   (set (is-player player)
        (is-door door))
   #:obstructors
   (set (is-closed door))
   #:additions
   (set (is-closed door))))

(define door-open-player-left
  (set (is-player player)
       (is-room left-room)
       (is-room right-room)
       (is-door door)
       (at player left-room)
       (is-empty right-room)))

(define door-open-player-right
  (set (is-player player)
       (is-room left-room)
       (is-room right-room)
       (is-door door)
       (at player right-room)
       (is-empty left-room)))

(define door-closed-player-left
  (set (is-player player)
       (is-room left-room)
       (is-room right-room)
       (is-door door)
       (at player left-room)
       (is-empty right-room)
       (is-closed door)))

(define door-closed-player-right
  (set (is-player player)
       (is-room left-room)
       (is-room right-room)
       (is-door door)
       (at player right-room)
       (is-empty left-room)
       (is-closed door)))

(module+ test
  (test-case "action-execute"
    (check-equal? (action-execute move-left door-open-player-right)
                  door-open-player-left)
    (check-equal? (action-execute move-right door-open-player-left)
                  door-open-player-right)))
