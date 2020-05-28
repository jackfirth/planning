#lang scribble/manual

@(require (for-label planning/examples/sokoban
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
    #:public (list 'planning/examples/sokoban
                   'planning/hash/action
                   'planning/hash/goal
                   'planning/hash/problem
                   '(submod planning/hash/visualize headless)
                   'rebellion/collection/multidict)
    #:private (list 'racket/base)))

@title{Sokoban}
@defmodule[planning/examples/sokoban]

Sokoban is a puzzle game in which the player must push crates around a
warehouse, trying to put them in storage locations. It was created in 1981 and
has since found a home in the AI research community as a useful problem domain
for testing @tech{automated planners}.

The Sokoban world is a rectangular grid containing walls, crates, a player, and
storage locations. The goal is to push the crates to the storage locations. The
player can move around the world and push crates up, down, left, and right, but
only if the crate isn't blocked by a wall or another crate.

@(examples
  #:eval (make-evaluator) #:once
  (eval:no-prompt
   (define state
     (hash (space 0 0) wall
           (space 0 1) wall
           (space 0 2) wall
           (space 0 3) wall
           (space 0 4) wall
           (space 1 4) wall
           (space 2 4) wall
           (space 3 4) wall
           (space 4 4) wall
           (space 4 3) wall
           (space 4 2) wall
           (space 4 1) wall
           (space 4 0) wall
           (space 3 0) wall
           (space 2 0) wall
           (space 1 0) wall
           (space 1 1) player
           (space 2 2) crate
           (space 3 3) storage-location))
   (define problem
     (hash-planning-problem
      #:state state
      #:actions (sokoban-possible-actions state)
      #:goal sokoban-goal)))

  (hash-visualize-plan! problem #:draw-state-with sokoban-pict))
