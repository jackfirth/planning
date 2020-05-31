#lang racket/base

(require planning/multiset/action
         planning/multiset/condition
         rebellion/base/range
         rebellion/collection/multiset
         rebellion/type/enum)

;@------------------------------------------------------------------------------

(define-enum-type factorio-item
  (iron-plate iron-gear-wheel copper-plate copper-wire electronic-circuit))

(define craft-iron-gear
  (multiset-action
   #:preconditions (hash iron-plate (singleton-range 2))
   #:deletions (multiset iron-plate iron-plate)
   #:additions (multiset iron-gear-wheel)
   #:cost 1/2))

(module+ main
  (multiset-condition-ignore-frequencies
   (multiset-condition (hash iron-plate (singleton-range 2)))))
