#lang racket/base

(require planning/hash/action
         planning/hash/condition
         planning/hash/problem
         racket/set
         rebellion/collection/hash
         rebellion/collection/multidict
         rebellion/type/enum)

(module+ test
  (require (submod "..")
           rackunit
           rebellion/base/option))

;@------------------------------------------------------------------------------
;; The Room World contains two rooms, a door between them, and a player object
;; who resides in one of the rooms. The player may move between rooms if the
;; door is open, as well as open and close the door. This simple world is mainly
;; useful for planning demonstrations and testing.

(define-enum-type object (player door))
(define-enum-type place (left-room right-room doorway))

(define move-left
  (hash-action #:requirements (multidict right-room player)
               #:obstructions (multidict doorway door)
               #:obstructing-keys (set left-room)
               #:deletions (set right-room)
               #:additions (hash left-room player)))

(define move-right
  (hash-action #:requirements (multidict left-room player)
               #:obstructions (multidict doorway door)
               #:obstructing-keys (set right-room)
               #:deletions (set left-room)
               #:additions (hash right-room player)))

(define close-door
  (hash-action #:obstructing-keys (set doorway)
               #:additions (hash doorway door)))

(define open-door
  (hash-action #:requirements (multidict doorway door)
               #:deletions (set doorway)))

(module+ test
  (test-case "move-left"
    (test-case "door is open"
      (define world (hash right-room player))
      (check-true (hash-action-applicable? move-left world))
      (check-equal? (hash-act world move-left) (hash left-room player)))

    (test-case "door is closed"
      (define world (hash right-room player doorway door))
      (check-false (hash-action-applicable? move-left world))))

  (test-case "move-right"
    (test-case "door is open"
      (define world (hash left-room player))
      (check-true (hash-action-applicable? move-right world))
      (check-equal? (hash-act world move-right) (hash right-room player)))

    (test-case "door is closed"
      (define world (hash left-room player doorway door))
      (check-false (hash-action-applicable? move-right world))))

  (test-case "close-door"
    (check-true (hash-action-applicable? close-door empty-hash))
    (check-equal? (hash-act empty-hash close-door) (hash doorway door)))

  (test-case "open-door"
    (define world (hash doorway door))
    (check-true (hash-action-applicable? open-door world))
    (check-equal? (hash-act world open-door) empty-hash))

  (test-case "planning"
    (define problem
      (hash-planning-problem
       #:state (hash left-room player doorway door)
       #:actions (set move-left move-right close-door open-door)
       #:goal (hash-condition #:requirements (multidict right-room player))))
    (define expected-plan (list open-door move-right))
    (define actual (hash-plan problem))
    (check-pred present? actual)
    (check-equal? (present-value actual) expected-plan)))
