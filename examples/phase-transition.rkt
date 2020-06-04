#lang racket/base

(require racket/contract/base)

(provide
 (contract-out
  [state-of-matter? predicate/c]
  [solid state-of-matter?]
  [liquid state-of-matter?]
  [gas state-of-matter?]
  [melt set-action?]
  [freeze set-action?]
  [condense set-action?]
  [evaporate set-action?]
  [sublime set-action?]
  [deposit set-action?]
  [partially-melt set-action?]
  [partially-freeze set-action?]
  [partially-condense set-action?]
  [partially-evaporate set-action?]
  [partially-sublime set-action?]
  [partially-deposit set-action?]))

(require planning/set/action
         racket/set
         rebellion/type/enum)

;@------------------------------------------------------------------------------

(define-enum-type state-of-matter (solid liquid gas))

(define (total-phase-transition #:from start #:to end)
  (set-action #:requirements (set start)
              #:deletions (set start)
              #:additions (set end)))

(define (partial-phase-transition #:from start #:to end)
  (set-action #:requirements (set start) #:additions (set end)))

(define melt (total-phase-transition #:from solid #:to liquid))
(define freeze (total-phase-transition #:from liquid #:to solid))
(define condense (total-phase-transition #:from gas #:to liquid))
(define evaporate (total-phase-transition #:from liquid #:to gas))
(define sublime (total-phase-transition #:from solid #:to gas))
(define deposit (total-phase-transition #:from gas #:to solid))

(define partially-melt (partial-phase-transition #:from solid #:to liquid))
(define partially-freeze (partial-phase-transition #:from liquid #:to solid))
(define partially-condense (partial-phase-transition #:from gas #:to liquid))
(define partially-evaporate (partial-phase-transition #:from liquid #:to gas))
(define partially-sublime (partial-phase-transition #:from solid #:to gas))
(define partially-deposit (partial-phase-transition #:from gas #:to solid))
