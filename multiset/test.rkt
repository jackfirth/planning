#lang racket/base

(module+ test
  (require fancy-app
           planning/multiset/action
           planning/multiset/condition
           planning/multiset/problem
           racket/set
           rackunit
           rebellion/base/option
           rebellion/base/range
           rebellion/collection/entry
           rebellion/collection/multiset
           rebellion/streaming/reducer
           rebellion/streaming/transducer
           rebellion/type/enum))

;@------------------------------------------------------------------------------

(module+ test
  (define (multiset-of-copies . elements+frequencies)
    (transduce
     elements+frequencies
     batching-into-entries
     #:into
     (make-fold-reducer
      (λ (set e) (multiset-add set (entry-key e) #:copies (entry-value e)))
      empty-multiset)))

  (define-enum-type material (copper silver gold))

  (define transmute-copper-into-silver
    (multiset-action #:preconditions (hash copper (at-least-range 10))
                     #:deletions (multiset-of-copies copper 10)
                     #:additions (multiset silver)))

  (define transmute-silver-into-gold
    (multiset-action #:preconditions (hash silver (at-least-range 10))
                     #:deletions (multiset-of-copies silver 10)
                     #:additions (multiset gold)))

  (test-case "multiset-planning-problem-solve"
    (define problem
      (multiset-planning-problem
       #:state (multiset-of-copies copper 100)
       #:actions (set transmute-copper-into-silver transmute-silver-into-gold)
       #:goal (multiset-condition (hash gold (singleton-range 1)))))
    (check-equal?
     (multiset-plan problem)
     (present
      (list transmute-copper-into-silver
            transmute-copper-into-silver
            transmute-copper-into-silver
            transmute-copper-into-silver
            transmute-copper-into-silver
            transmute-copper-into-silver
            transmute-copper-into-silver
            transmute-copper-into-silver
            transmute-copper-into-silver
            transmute-copper-into-silver
            transmute-silver-into-gold)))))
