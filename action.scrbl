#lang scribble/manual

@(require (for-label planning/action
                     racket/base
                     racket/contract/base
                     racket/set
                     rebellion/collection/set)
          (submod planning/private doc))

@title{Actions}
@defmodule[planning/action]

An @deftech{action} is something an agent can do to change the state of the
world. In automated planning, a @deftech{factored representation} is used to
represent the world state as a set of independent statements of truth. Actions
are defined in terms of these statements, called @deftech{state fluents} or
simply @deftech{fluents}. Any value can be used as a fluent, so any
@racket-reference-tech{set} can serve as a world state. Each action is made up
of four sets of state fluents:

@itemlist[
 #:style 'ordered

 @item{@deftech{requirements}, which must be present in the world state in order
  to apply the action}

 @item{@deftech{obstructors}, which prevent the action from being applied if
 they are present in the world state}

 @item{@deftech{additions}, which are added to the world state as a result of
  applying the action}

 @item{@deftech{deletions}, which are removed from the world state as a result
  of applying the action}]

An action is @deftech{applicable} in a state if all of the action's requirements
are present and all of it's obstructors are absent. The requirements and
obstructors together define an action's @deftech{preconditions}. Meanwhile, the
@deftech{effects} of an action are its additions and deletions.

@defproc[(action? [v any/c]) boolean?]{
 A predicate for @tech{actions}.}

@defproc[(action [#:requirements requirements set? empty-set]
                 [#:obstructors obstructors set? empty-set]
                 [#:additions additions set? empty-set]
                 [#:deletions deletions set? empty-set])
         action?]{
 Constructs an action from the given @tech{requirements}, @tech{obstructors},
 @tech{additions}, and @tech{deletions}. The given sets must satisfy the
 following conditions:

 @itemlist[
 @item{The @racket[requirements] and @racket[obstructors] sets cannot have any
   values in common as that would create an action whose @tech{preconditions}
   are impossible to satisfy.}

 @item{The @racket[additions] and @racket[deletions] sets cannot have any values
   in common as that would create an action whose @tech{effects} are self
   contradictory.}

 @item{The @racket[requirements] and @racket[additions] sets cannot have any
   values in common because adding a @tech{fluent} that the action already
   requires be present would be redundant.}

 @item{The @racket[obstructors] and @racket[deletions] sets cannot have any
   values in common because an absent @tech{fluent} cannot be deleted.}]

 If any of the above conditions are violated, a contract exception is raised.}
