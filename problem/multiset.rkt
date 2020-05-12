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
         racket/match
         racket/sequence
         racket/set
         rebellion/base/option
         rebellion/collection/list
         rebellion/collection/multiset
         rebellion/collection/set
         rebellion/private/strict-cond
         rebellion/type/record
         rebellion/type/tuple)

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

(define-tuple-type plan (reversed-steps predicted-state))
(define empty-plan (plan empty-list _))

(define (initial-frontier initial-state) (list (empty-plan initial-state)))

(define (deterministic-search action-applicable? action-perform goal-achieved?)
  
  (define (plan-children node actions)
    (match-define (plan steps state) node)
    (for/list ([action (in-list actions)]
               #:when (action-applicable? action state))
      (plan (list-insert steps action) (action-perform action state))))

  (define (search actions initial-state goal)
    (let loop ([frontier (initial-frontier initial-state)] [expanded empty-set])
      (match frontier
        [(list) absent]
        [(cons (plan steps state) _)
         #:when (goal-achieved? goal state)
         (present (list-reverse steps))]
        [(cons (and node (plan steps state)) frontier-remaining)
         (define children (plan-children node))
         (define new-frontier (list-append frontier-remaining children))
         (loop new-frontier (set-add expanded node))])))

  search)
       

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
