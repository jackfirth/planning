#lang scribble/manual

@(require (for-label planning/hash/action
                     planning/hash/goal
                     planning/hash/problem
                     racket/base
                     racket/contract/base
                     racket/set
                     rebellion/collection/hash
                     rebellion/collection/multidict
                     rebellion/collection/set)
          (submod planning/private doc)
          scribble/decode
          scribble/example)

@(define make-evaluator
   (make-module-sharing-evaluator-factory
    #:public (list 'planning/hash/action
                   'planning/hash/goal
                   'planning/hash/problem)
    #:private (list 'racket/base)))

@title{The Hash State Representation}

In the @tech{hash state representation}, the world is represented by a
@reference-tech{hash table}. Actions and goals are represented by
@tech{hash actions} and @tech{hash goals}.

@section{Hash Actions}
@defmodule[planning/hash/action]

A @deftech{hash action} is an @tech{action} on @reference-tech{hash tables}.
Hash actions have several components, broadly grouped into preconditions and
effects. The preconditions of a hash action are:

@itemlist[
 @item{A @rebellion-tech{multidict} of @emph{requirements}. For each key and set
  of values in this multidict, the hash table must have a mapping from the key
  to one of the corresponding values. This can be used to express preconditions
  where many possible values for a key are allowed.

  For example, a videogame with the action "destroy the object at space A4"
  might have the requirements "space A4 must contain a rock, a cracked wall, or
  an enemy." These requirements would be modeled as a multidict mapping the A4
  space to each of the three destructible objects.}

 @item{A @reference-tech{set} of @emph{required keys}. The hash table must
  contain a mapping for each of these keys, but it does not matter what value
  the key is mapped to.}

 @item{A @reference-tech{set} of @emph{required values}. The hash table must
  contain at least one copy of each required value, but it does not matter which
  key (or keys) the hash table maps to the value.}

 @item{A @rebellion-tech{multidict} of @emph{obstructions}. For each key and set
  of values in this multidict, the hash table must @bold{not} contain a mapping
  from that key to any of the corresponding values. This can be used to express
  preconditions where a key may be present, but only if it's not mapped to
  certain values.}

 @item{A @reference-tech{set} of @emph{obstructing keys}. The hash table must
  @bold{not} contain any of these keys, regardless of what values they map to.}

 @item{A @reference-tech{set} of @emph{obstructing values}. The hash table must
  @bold{not} contain any of these values, regardless of what keys are mapped to
  them.}]

All of a hash action's preconditions must be satisfied for the action to be
@tech{applicable}. In that case, the effects of applying the action are defined
by:

@itemlist[
 @item{A hash table of @emph{additions}, whose entries are added to the hash
  begin acted upon. If the acted-upon hash already contains a mapping for any of
  the added keys, those mappings are overwritten.}

 @item{A @reference-tech{set} of @emph{deletions}, containing keys that are
  removed from the hash table being acted upon. Attempting to delete keys that
  do not exist in the hash table is not an error, and makes no changes to the
  hash table.}]

@defproc[(hash-action? [v any/c]) boolean?]{
 A predicate for @tech{hash actions}.}

@defproc[(hash-action [#:requirements requirements multidict? empty-multidict]
                      [#:required-keys required-keys set? empty-set]
                      [#:required-values required-values set? empty-set]
                      [#:obstructions obstructions multidict? empty-multidict]
                      [#:obstructing-keys obstructing-keys set? empty-set]
                      [#:obstructing-values obstructing-values set? empty-set]
                      [#:additions additions immutable-hash? empty-hash]
                      [#:deletions deletions set? empty-set])
         hash-action?]{
 Constructs a @tech{hash action}.}

@(define see-hash-action-definition
   (decode-content
    @list{See the definition of @tech{hash actions} for an explanation.}))

@defproc[(hash-action-requirements [action hash-action?]) multidict?]{
 Returns the requirements of @racket[action]. @see-hash-action-definition}

@defproc[(hash-action-required-keys [action hash-action?]) set?]{
 Returns the required keys of @racket[action]. @see-hash-action-definition}

@defproc[(hash-action-required-values [action hash-action?]) set?]{
 Returns the required values of @racket[action]. @see-hash-action-definition}

@defproc[(hash-action-obstructions [action hash-action?]) multidict?]{
 Returns the obstructions of @racket[action]. @see-hash-action-definition}

@defproc[(hash-action-obstructing-keys [action hash-action?]) set?]{
 Returns the obstructing keys of @racket[action]. @see-hash-action-definition}

@defproc[(hash-action-obstructing-values [action hash-action?]) set?]{
 Returns the obstructing values of @racket[action]. @see-hash-action-definition}

@defproc[(hash-action-additions [action hash-action?]) immutable-hash?]{
 Returns the additions of @racket[action]. @see-hash-action-definition}

@defproc[(hash-action-deletions [action hash-action?]) set?]{
 Returns the deletions of @racket[action]. @see-hash-action-definition}

@section{Hash Goals}
@defmodule[planning/hash/goal]

A @deftech{hash goal} is a @tech{goal} in the @tech{hash state representation}.

@defproc[(hash-goal? [v any/c]) boolean?]{
 A predicate for @tech{hash goals}.}

@section{Hash Planning Problems}
@defmodule[planning/hash/problem]

A @deftech{hash planning problem} is a combination of a
@reference-tech{hash table}, a set of @tech{hash actions}, and a
@tech{hash goal}. A solution to the problem is a list of actions to perform
that will transform the hash table into a hash table that satisfies the goal.

@defproc[(hash-planning-problem? [v any/c]) boolean?]{
 A predicate for @tech{hash planning problems}.}
