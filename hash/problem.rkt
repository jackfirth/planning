#lang racket/base

(require racket/contract/base)

(provide
 (contract-out
  [hash-plan (-> hash-planning-problem? (option/c (listof hash-action?)))]
  [hash-planning-problem
   (-> #:state immutable-hash?
       #:actions (set/c hash-action?)
       #:goal hash-goal?
       hash-planning-problem?)]
  [hash-planning-problem? predicate/c]
  [hash-planning-problem-state (-> hash-planning-problem? immutable-hash?)]
  [hash-planning-problem-actions
   (-> hash-planning-problem? (set/c hash-action?))]
  [hash-planning-problem-goal (-> hash-planning-problem? hash-goal?)]))

(require fancy-app
         planning/hash/action
         planning/hash/goal
         planning/private
         racket/match
         racket/set
         racket/sequence
         rebellion/base/option
         rebellion/collection/hash
         rebellion/collection/list
         rebellion/collection/set
         rebellion/streaming/reducer
         rebellion/streaming/transducer
         rebellion/type/record
         rebellion/type/tuple)

;@------------------------------------------------------------------------------
;; The current planner uses a naive breadth-first search approach that ignores
;; action costs. It will find the shortest plan if one exists, but not
;; necessarily the cheapest. It is not effective on problems with large state
;; spaces. The planner is guaranteed to terminate if a solution exists or the
;; state space is finite.

(define-record-type hash-planning-problem (state actions goal))

(define-tuple-type plan (reversed-steps predicted-state))
(define empty-plan (plan empty-list _))

(define (plan-length p) (list-size (plan-reversed-steps p)))

;; A frontier is a set of plans. It represents the collection of nodes that have
;; unexplored children.

(define (initial-frontier initial-state) (set (empty-plan initial-state)))

(define-tuple-type selection (node remaining-frontier))

;; TODO: This is super inefficient, should use a queue instead.
(define (select-shallowest-node frontier)
  ;; TODO(https://github.com/jackfirth/rebellion/issues/428): This should use
  ;;   nonempty-into-min instead of present-value + into-min.
  (define shortest-plan
    (present-value
     (transduce frontier #:into (into-min #:key plan-length))))
  (selection shortest-plan (set-remove frontier shortest-plan)))

(define (plan-children node actions #:expanded expanded-states)
  (match-define (plan steps state) node)
  (for*/set ([action (in-list actions)]
             #:when (hash-action-applicable? action state)
             [predicted-state (in-value (hash-act state action))]
             #:unless (set-contains? expanded-states predicted-state))
    (plan (list-insert steps action) predicted-state)))

(define (breadth-first-search actions initial-state goal)
  ;; TODO: This should track and log some statistics about how many states were
  ;;   explored, the depth and width of the state tree, the ratio of applicable
  ;;   actions to inapplicable ones, etc. It's super hard to debug slow searches
  ;;   otherwise.
  (let loop ([frontier (initial-frontier initial-state)] [expanded empty-set])
    (cond
      [(empty-set? frontier) absent]
      [else
       (match-define
         (selection (and node (plan steps state)) remaining-frontier)
         (select-shallowest-node frontier))
       (cond
         [(hash-goal-achieved? goal state) (present (list-reverse steps))]
         [else
          (define new-expanded (set-add expanded state))
          (define children (plan-children node actions #:expanded expanded))
          (define new-frontier (set-union remaining-frontier children))
          (loop new-frontier new-expanded)])])))

(define (hash-plan problem)
  (define actions (sequence->list (hash-planning-problem-actions problem)))
  (define goal (hash-planning-problem-goal problem))
  (breadth-first-search actions (hash-planning-problem-state problem) goal))

