#lang racket/base

(require racket/contract/base)

(provide
 (contract-out
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

(require planning/hash/action
         planning/hash/goal
         racket/set
         rebellion/collection/hash
         rebellion/type/record)

;@------------------------------------------------------------------------------

(define-record-type hash-planning-problem (state actions goal))
