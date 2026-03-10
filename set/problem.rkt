#lang racket/base


(require racket/contract/base)


(provide
 (contract-out
  [set-plan (-> set-planning-problem? (or/c (listof set-action?) #false))]))


(require planning/set/action
         racket/treelist)


;@----------------------------------------------------------------------------------------------------


(struct set-literal () #:transparent)
(struct present-element set-literal (value) #:transparent)
(struct absent-element set-literal (value) #:transparent)


(struct set-planning-graph
  (initial-state  #| set? |#
   levels  #| (treelist/c set-planning-graph-level?) |#)
  #:transparent)


(struct set-planning-graph-level
  (actions  #| (set/c action) |#
   results  #| (set/c set-literal) |#
   action-mutex-links  #| (hash/c action (set/c action)) |#
   precondition-action-links  #| (hash/c set-literal (set/c action)) |#
   postcondition-action-links  #| (hash/c set-literal (set/c action)) |#))


(struct set-planning-problem
  (initial-state  #| set? |#
   actions  #| (set/c set-action?) |#
   goal  #| set-condition? |#)
  #:transparent)


(define (initial-set-planning-graph problem)
  (set-planning-graph (set-planning-problem-initial-state problem) (treelist)))


(define (set-planning-graph-expand graph problem)
  graph)


(define (set-plan problem)
  #false)
