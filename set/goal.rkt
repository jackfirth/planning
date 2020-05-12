#lang racket/base

(require racket/contract/base)

(provide
 (contract-out
  [set-goal? predicate/c]
  [set-goal
   (->i ()
        (#:requirements [requirements set?]
         #:obstructors [obstructors set?])
        [_ set-goal?])]
  [set-goal-requirements (-> set-goal? set?)]
  [set-goal-obstructors (-> set-goal? set?)]
  [set-goal-achieved? (-> set-goal? set? boolean?)]))

(require planning/private
         racket/set
         rebellion/collection/set
         rebellion/type/record)

;@------------------------------------------------------------------------------

(define-record-type set-goal (requirements obstructors)
  #:omit-root-binding)

(define (set-goal #:requirements [requirements empty-set]
                  #:obstructors [obstructors empty-set])
  (constructor:set-goal #:requirements requirements
                        #:obstructors obstructors))

(define (set-goal-achieved? goal set)
  (and (set-contains-all? set (set-goal-requirements goal))
       (set-contains-none? set (set-goal-obstructors goal))))
