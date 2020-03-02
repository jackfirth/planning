#lang racket/base

(require racket/contract/base)

(provide
 (contract-out
  [room-world-object? predicate/c]
  [player room-world-object?]
  [left-room room-world-object?]
  [right-room room-world-object?]
  [door room-world-object?]
  [is-player? predicate/c]
  [is-player (-> room-world-object? is-player?)]
  [is-player-claimed-player (-> is-player? room-world-object?)]))

;; TODO: add provides for the rest of the stuff in this module

(require planning/action
         racket/set
         rebellion/type/enum
         rebellion/type/tuple)

(module+ test
  (require (submod "..")
           rackunit))

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
