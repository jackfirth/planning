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

@defproc[(action-applicable? [act action?] [state set?]) boolean?]{
 Determines whether @racket[act] is @tech{applicable} in @racket[state]. To be
 applicable, @racket[state] must contain every value in @racket[
 (action-requirements act)] and none of the values in @racket[
 (action-obstructors act)].}

@defproc[(action-execute [act action?] [state set?]) set?]{
 Executes @racket[act] in @racket[state], returning an updated state. The given
 @racket[act] must be @tech{applicable} in @racket[state], or else a contract
 error is raised. The updated state contains all of the same @tech{fluents} as
 @racket[state], minus the @tech{deletions} of @racket[act] and plus the @tech{
  additions} of @racket[act].}

@defproc[(action-requirements [act action?]) set?]{
 Returns the @tech{requirements} of @racket[act], which is the set of @tech{
  fluents} that must be present in the state in order for the action to be
 @tech{applicable}.}

@defproc[(action-obstructors [act action?]) set?]{
 Returns the @tech{obstructors} of @racket[act], which is the set of @tech{
  fluents} that cannot be present in the state in order for the action to be
 @tech{applicable}.}

@defproc[(action-additions [act action?]) set?]{
 Returns the @tech{additions} of @racket[act], which is the set of @tech{
  fluents} that are added to the state when the action is applied.}

@defproc[(action-deletions [act action?]) set?]{
 Returns the @tech{deletions} of @racket[act], which is the set of @tech{
  fluents} that are removed from the state when the action is applied.}

@defproc[(action-preconditions [act action?])
         (hash/c any/c precondition-fluent-type?)]{
 Returns the @tech{preconditions} of @racket[act], which is the action's @tech{
  requirements} and @tech{obstructors}. The returned hash maps @tech{fluents} to
 which type of precondition the fluent is.}

@deftogether[[
 @defproc[(precondition-fluent-type? [v any/c]) boolean?]
 @defthing[required-fluent precondition-fluent-type?]
 @defthing[obstructing-fluent precondition-fluent-type?]]]{
 An @rebellion-tech{enum type} for the two types of @tech{fluents} an action can
 have in its @tech{preconditions}.}

@defproc[(action-effects [act action?])
         (hash/c any/c effect-fluent-type?)]{
 Returns the @tech{effects} of @racket[act], which is the action's @tech{
  additions} and @tech{deletions}. The returned hash maps @tech{fluents} to
 which type of effect the fluent is.}

@deftogether[[
 @defproc[(effect-fluent-type? [v any/c]) boolean?]
 @defthing[added-fluent effect-fluent-type?]
 @defthing[deleted-fluent effect-fluent-type?]]]{
 An @rebellion-tech{enum type} for the two types of @tech{fluents} an action can
 have in its @tech{effects}.}
