#lang scribble/manual

@(require (for-label planning/examples/sokoban
                     planning/hash/action
                     planning/hash/goal
                     planning/hash/problem
                     racket/base
                     racket/contract/base
                     racket/math)
          (submod planning/private doc)
          scribble/example)

@(define make-evaluator
   (make-module-sharing-evaluator-factory
    #:public (list 'planning/examples/sokoban
                   'planning/hash/action
                   'planning/hash/goal
                   'planning/hash/problem
                   '(submod planning/hash/visualize headless))
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

@defproc[(space? [v any/c]) boolean?]{
 A predicate for grid spaces in a Sokoban level. A space is a pair of an X
 coordinate and a Y coordinate.}

@defproc[(space [x natural?] [y natural?]) space?]{
 Constructs a grid space with @racket[x] and @racket[y] coordinates. The origin
 @racket[(space 0 0)] represents the @bold{top left corner} of a Sokoban level.}

@deftogether[[
 @defproc[(space-x [s space?]) natural?]
 @defproc[(space-y [s space?]) natural?]]]{
 Returns the X and Y coordinates of @racket[s], respectively.}

@defproc[(sokoban-object? [v any/c]) boolean?]{
 A predicate for objects in the Sokoban world.}

@deftogether[[
 @defthing[wall sokoban-object?]
 @defthing[player sokoban-object?]
 @defthing[crate sokoban-object?]
 @defthing[storage-location sokoban-object?]
 @defthing[player-in-storage sokoban-object?]
 @defthing[crate-in-storage sokoban-object?]]]{
 The various objects that can exist in a Sokoban level. The
 @racket[player-in-storage] and @racket[crate-in-storage] objects are like the
 @racket[player] and @racket[crate] objects, except they represent cases where
 the player or crate is standing on top of a storage location space. Distinct
 objects for these cases are necessary because the Sokoban world uses the
 @tech{hash state representation}, so a single cannot contain multiple objects.}
