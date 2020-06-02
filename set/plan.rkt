#lang racket/base

(require racket/contract/base)

(provide
 (contract-out
  [set-plan
   (-> set? (sequence/c set-action?) set-condition?
       (option/c (listof set-action?)))]
  [set-plan!
   (->* (set? (sequence/c set-action?) set-condition?
              #:draw-state-with (-> set? pict?))
        (#:frames-per-second (and/c rational? positive? exact?))
       (option/c animation?))]))

(require fancy-app
         pict
         planning/private
         planning/private/animation
         planning/set/action
         planning/set/condition
         racket/match
         racket/sequence
         racket/set
         rebellion/base/option
         rebellion/collection/list
         rebellion/collection/set
         rebellion/streaming/reducer
         rebellion/streaming/transducer
         rebellion/type/tuple)

;@------------------------------------------------------------------------------

;; Most of this was copied from the hash planner implementation.

(define-tuple-type plan (reversed-steps predicted-state))
(define empty-plan (plan empty-list _))

(define (plan-length p) (list-size (plan-reversed-steps p)))

(define (initial-frontier initial-state) (set (empty-plan initial-state)))

(define-tuple-type selection (node remaining-frontier))

(define (select-shallowest-node frontier)
  (define shortest-plan
    (present-value
     (transduce frontier #:into (into-min #:key plan-length))))
  (selection shortest-plan (set-remove frontier shortest-plan)))

(define (plan-children node actions #:expanded expanded-states)
  (match-define (plan steps state) node)
  (for*/set ([action (in-list actions)]
             #:when (set-action-applicable? action state)
             [predicted-state (in-value (set-act state action))]
             #:unless (set-contains? expanded-states predicted-state))
    (plan (list-insert steps action) predicted-state)))

(define (set-plan initial-state actions goal)
  (define action-list (sequence->list actions))
  (let loop ([frontier (initial-frontier initial-state)] [expanded empty-set])
    (cond
      [(empty-set? frontier) absent]
      [else
       (match-define
         (selection (and node (plan steps state)) remaining-frontier)
         (select-shallowest-node frontier))
       (cond
         [(set-meets-condition? state goal) (present (list-reverse steps))]
         [else
          (define new-expanded (set-add expanded state))
          (define children (plan-children node action-list #:expanded expanded))
          (define new-frontier (set-add-all remaining-frontier children))
          (loop new-frontier new-expanded)])])))

(define (set-animation set actions
                       #:draw-state-with state-drawer
                       #:frames-per-second [fps 4])
  (define post-action-states
    (transduce actions (folding set-act set) #:into into-list))
  (transduce (list-insert post-action-states set)
             (mapping state-drawer)
             #:into (into-animation #:frame-rate fps)))

(define (set-plan! initial-state actions goal
                   #:draw-state-with state-drawer
                   #:frames-per-second [fps 4])
  (option-map
   (set-plan initial-state actions goal)
   (set-animation initial-state _
                  #:draw-state-with state-drawer
                  #:frames-per-second fps)))
