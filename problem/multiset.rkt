#lang racket/base

(require racket/contract/base)

(provide
 (contract-out
  [multiset-planning-problem
   (-> #:state multiset?
       #:actions (sequence/c multiset-action?)
       #:goal multiset-goal?
       multiset-planning-problem?)]
  [multiset-planning-problem? predicate/c]
  [multiset-planning-problem-state
   (-> multiset-planning-problem? multiset?)]
  [multiset-planning-problem-actions
   (-> multiset-planning-problem? (set/c multiset-action?))]
  [multiset-planning-problem-goal
   (-> multiset-planning-problem? multiset-goal?)]
  [multiset-planning-problem-solve
   (-> multiset-planning-problem? (option/c (listof multiset-action?)))]))

(require fancy-app
         planning/action/multiset
         planning/goal/multiset
         planning/private
         racket/sequence
         racket/set
         rebellion/base/option
         rebellion/collection/list
         rebellion/collection/multiset
         rebellion/private/strict-cond
         rebellion/type/record)

;@------------------------------------------------------------------------------

(define-record-type multiset-planning-problem (state actions goal)
  #:omit-root-binding)

(define (multiset-planning-problem #:state state
                                   #:actions actions
                                   #:goal goal)
  (constructor:multiset-planning-problem
   #:state state
   #:actions (sequence->set actions)
   #:goal goal))

(define (multiset-planning-problem-solve problem)
  (define actions (sequence->list (multiset-planning-problem-actions problem)))
  (define goal (multiset-planning-problem-goal problem))
  (let loop ([state (multiset-planning-problem-state problem)]
             [unconsidered-actions actions])
    (cond
      [(multiset-goal-achieved? goal state) (present empty-list)]
      [(empty-list? unconsidered-actions) absent]
      [else
       (define next-action (list-first unconsidered-actions))
       (cond
         [(multiset-action-applicable? next-action state)
          (define next-state (multiset-action-perform next-action state))
          (option-case
           (loop next-state actions)
           #:present (λ (plan) (present (list-insert plan next-action)))
           #:absent (λ () (loop state (list-rest unconsidered-actions))))]
         [else (loop state (list-rest unconsidered-actions))])])))
