#lang scribble/manual

@(require (for-label pict
                     planning/examples/sokoban
                     planning/hash/action
                     planning/hash/goal
                     planning/hash/problem
                     racket/base
                     racket/contract/base
                     racket/set
                     racket/math)
          (submod planning/private doc)
          scribble/example)

@(define make-evaluator
   (make-module-sharing-evaluator-factory
    #:public (list 'planning/examples/sokoban
                   'planning/hash/action
                   'planning/hash/goal
                   'planning/hash/problem
                   '(submod planning/hash/visualize headless)
                   'racket/set)
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
 @tech{hash state representation}, so a single space cannot contain multiple
 objects.}

@defproc[(sokoban-applicable-actions [state (hash/c space? sokoban-object?)])
         (set/c hash-action?)]{
 Constructs a set of all actions that are @tech{applicable} in @racket[state].

 @(examples
   #:eval (make-evaluator) #:once
   (eval:no-prompt
    (define state
      (hash (space 0 0) wall
            (space 0 1) wall
            (space 0 2) wall
            (space 0 3) wall
            (space 1 3) wall
            (space 2 3) wall
            (space 3 3) wall
            (space 4 3) wall
            (space 4 2) wall
            (space 4 1) wall
            (space 4 0) wall
            (space 3 0) wall
            (space 2 0) wall
            (space 1 0) wall
            (space 1 1) player
            (space 2 2) crate
            (space 3 2) storage-location)))

   (sokoban-pict state)
   (set-count (sokoban-applicable-actions state)))}

@defproc[(sokoban-possible-actions
          [initial-state (hash/c space? sokoban-object?)])
         (set/c hash-action?)]{
 Constructs a set of all possible actions that could be performed over the
 course of a Sokoban game starting from @racket[initial-state]. This includes
 actions that are not mmediately applicable, but which can be applied if other
 actions are
 performed first.

 The set of possible actions returned is an @emph{optimistic estimate}. Every
 possible action is returned, but some impossible actions may also be returned.
 An estimate is returned because computing the exact set of possible actions can
 be complex and prohibitively expensive.

 @(examples
   #:eval (make-evaluator) #:once
   (eval:no-prompt
    (define state
      (hash (space 0 0) wall
            (space 0 1) wall
            (space 0 2) wall
            (space 0 3) wall
            (space 1 3) wall
            (space 2 3) wall
            (space 3 3) wall
            (space 4 3) wall
            (space 4 2) wall
            (space 4 1) wall
            (space 4 0) wall
            (space 3 0) wall
            (space 2 0) wall
            (space 1 0) wall
            (space 1 1) player
            (space 2 2) crate
            (space 3 2) storage-location)))
   
   (sokoban-pict state)
   (set-count (sokoban-possible-actions state)))}

@; TODO(https://github.com/jackfirth/planning/issues/2): Use hash-goal/c here.
@defthing[sokoban-goal hash-goal?
          #:value (hash-goal #:obstructing-values (set crate))]{
 The @tech{goal} of every Sokoban level is the same: push every crate into
 storage. Because pushing a @racket[crate] into a storage location changes it
 into a @racket[crate-in-storage], this means that all that's required to win is
 for the Sokoban level's hash table to not contain any @racket[crate] values.}

@defproc[(sokoban-pict [state (hash/c space? sokoban-object?)]) pict?]{
 Draws a picture of @racket[state].

 @(examples
   #:eval (make-evaluator) #:once
   (sokoban-pict
    (hash (space 0 0) player
          (space 1 3) crate
          (space 3 1) crate
          (space 0 4) storage-location
          (space 4 3) storage-location)))}
