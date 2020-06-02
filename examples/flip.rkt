#lang racket/base

(require racket/contract/base)

(provide
 (contract-out
  [space? predicate/c]
  [space (-> x-coordinate/c y-coordinate/c space?)]
  [space-x (-> space? x-coordinate/c)]
  [space-y (-> space? y-coordinate/c)]
  [flip-actions (set/c set-action?)]
  [flip-level? predicate/c]
  [flip-level (-> (sequence/c space?) flip-level?)]
  [flip-level-light-spaces (-> flip-level? (set/c space?))]
  [flip-level-plan! (-> flip-level? (option/c animation?))]))

(require fancy-app
         pict
         planning/private
         planning/private/animation
         planning/set/action
         planning/set/condition
         planning/set/plan
         point-free
         racket/list
         racket/match
         racket/pretty
         racket/sequence
         racket/set
         rebellion/base/option
         rebellion/collection/list
         rebellion/streaming/reducer
         rebellion/streaming/transducer
         rebellion/type/enum
         rebellion/type/tuple)

(module+ test
  (require (submod "..")
           rackunit))

;@------------------------------------------------------------------------------

(define-enum-type direction (up down left right))
(define directions (set up down left right))

(define width 3)
(define height 3)
(define max-x (sub1 width))
(define max-y (sub1 height))
(define x-coordinate/c (integer-in 0 max-x))
(define y-coordinate/c (integer-in 0 max-y))

(define-tuple-type space (x y))

(define spaces
  (for*/set ([x (in-range 0 width)]
             [y (in-range 0 height)])
    (space x y)))

(define (space-neighbor s dir)
  (match* (s dir)
    [((space x 0) (== up)) absent]
    [((space x (== max-y)) (== down)) absent]
    [((space 0 y) (== left)) absent]
    [((space (== max-x) y) (== right)) absent]
    [((space x y) (== up)) (present (space x (sub1 y)))]
    [((space x y) (== down)) (present (space x (add1 y)))]
    [((space x y) (== left)) (present (space (sub1 x) y))]
    [((space x y) (== right)) (present (space (add1 x) y))]))

(define (space-neighborhood s)
  (define neighbors
    (for*/set ([dir (in-set directions)]
               [neighbor (in-option (space-neighbor s dir))])
      neighbor))
  (set-add neighbors s))

(define-tuple-type flip-level (light-spaces) #:omit-root-binding)

(define (flip-level light-spaces)
  (constructor:flip-level (sequence->set light-spaces)))

(define (flip-action light-to-dark-spaces dark-to-light-spaces)
  (set-action
   #:requirements light-to-dark-spaces
   #:obstructions dark-to-light-spaces
   #:deletions light-to-dark-spaces
   #:additions dark-to-light-spaces))

(define (flip-actions-at-space s)
  (define spaces (space-neighborhood s))
  (for/set ([light-to-dark (in-combinations (sequence->list spaces))])
    (define dark-to-light (set-remove-all spaces light-to-dark))
    (flip-action light-to-dark dark-to-light)))             

(define flip-actions
  (for*/set ([s (in-immutable-set spaces)]
             [action (flip-actions-at-space s)])
    action))

(define flip-goal (set-condition #:requirements spaces))

(define light-space-pict (filled-rectangle 32 32 #:color "white"))

(define (flip-board-pict light-spaces)
  (for/fold ([bkg (blank (* 32 width) (* 32 height))])
            ([s (in-immutable-set light-spaces)])
    (define dx (* (space-x s) 32))
    (define dy (* (space-y s) 32))
    (pin-over bkg dx dy light-space-pict)))

(define (flip-level-pict level)
  (flip-board-pict (flip-level-light-spaces level)))

(define (flip-level-plan level)
  (set-plan (flip-level-light-spaces level) flip-actions flip-goal))

(define (flip-level-plan! level)
  (set-plan! (flip-level-light-spaces level) flip-actions flip-goal
             #:draw-state-with flip-board-pict
             #:frames-per-second 1))

(define (flip-at level s)
  (define light-spaces (flip-level-light-spaces level))
  (for/first ([action (flip-actions-at-space s)]
              #:when (set-action-applicable? action light-spaces))
    (flip-level (set-act light-spaces action))))

(define (flip-at! level s)
  (flip-level-pict (flip-at level s)))

(define completed-level (flip-level spaces))

(define level1 (flip-at completed-level (space 1 2)))

(define level2
  (~> completed-level
      (flip-at _ (space 0 1))
      (flip-at _ (space 1 2))))

(module+ test
  (test-case "level2"
    (define plan (flip-level-plan level2))
    (check-pred present? plan)
    (check-equal? (list-size (present-value plan)) 2)))
