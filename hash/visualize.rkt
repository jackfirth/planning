#lang racket/base

(require racket/contract/base)

(provide
 (contract-out
  [hash-visualize-plan!
   (->* (hash-planning-problem? #:draw-state-with (-> immutable-hash? pict?))
        (#:frame-rate (and/c rational? positive? exact?))
        animation?)]))

(require pict
         planning/hash/action
         planning/hash/goal
         planning/hash/problem
         planning/private/animation
         rebellion/base/option
         rebellion/collection/hash
         rebellion/collection/list
         rebellion/streaming/reducer
         rebellion/streaming/transducer)

;@------------------------------------------------------------------------------

(define (hash-visualize-plan! problem
                              #:draw-state-with state-drawer
                              #:frame-rate [frame-rate 2])
  ;; TODO(https://github.com/jackfirth/rebellion/issues/346): Skip the
  ;;   intermediate list by inserting the initial state directly into the
  ;;   stream and fusing the two stream pipelines.
  (define initial-state (hash-planning-problem-state problem))
  (define states
    (list-insert
     (transduce (present-value (hash-plan problem))
                (folding hash-act initial-state)
                #:into into-list)
     initial-state))
  (transduce states
             (mapping state-drawer)
             #:into (into-animation #:frame-rate frame-rate)))

(module test racket/base)

(module headless racket/base

  (require racket/contract/base)

  (provide
   (contract-out
    [hash-visualize-plan!
     (->* (hash-planning-problem? #:draw-state-with (-> immutable-hash? pict?))
          (#:frame-rate (and/c rational? positive? exact?))
          animation?)]))

  (require pict
           planning/hash/action
           planning/hash/goal
           planning/hash/problem
           (submod planning/private/animation headless)
           rebellion/base/option
           rebellion/collection/hash
           rebellion/collection/list
           rebellion/streaming/reducer
           rebellion/streaming/transducer)

  ;@----------------------------------------------------------------------------

  (define (hash-visualize-plan! problem
                                #:draw-state-with state-drawer
                                #:frame-rate [frame-rate 2])
    ;; TODO(https://github.com/jackfirth/rebellion/issues/346): Skip the
    ;;   intermediate list by inserting the initial state directly into the
    ;;   stream and fusing the two stream pipelines.
    (define initial-state (hash-planning-problem-state problem))
    (define states
      (list-insert
       (transduce (present-value (hash-plan problem))
                  (folding hash-act initial-state)
                  #:into into-list)
       initial-state))
    (transduce states
               (mapping state-drawer)
               #:into (into-animation #:frame-rate frame-rate))))
