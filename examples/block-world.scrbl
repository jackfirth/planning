#lang scribble/manual

@(require (for-label planning/examples/block-world
                     planning/hash/action
                     planning/hash/goal
                     planning/hash/problem
                     racket/base
                     racket/contract/base
                     rebellion/collection/multidict)
          (submod planning/private doc)
          scribble/example)

@(define make-evaluator
   (make-module-sharing-evaluator-factory
    #:public (list 'planning/examples/block-world
                   'planning/hash/action
                   'planning/hash/goal
                   'planning/hash/problem
                   '(submod planning/hash/visualize headless)
                   'rebellion/collection/multidict)
    #:private (list 'racket/base)))

@title{The Block World}
@defmodule[planning/examples/block-world]

@(examples
  #:eval (make-evaluator) #:once
  (eval:no-prompt
   (define world
     (hash (space 2 1) player
           (space 1 2) block))
  (define goal
    (hash-goal #:requirements (multidict (space 3 3) block (space 2 1) player)))
  (define problem
    (hash-planning-problem
     #:state world #:actions (block-world-actions 4 4) #:goal goal)))

  (hash-visualize-plan! problem #:draw-state-with block-world-pict))
