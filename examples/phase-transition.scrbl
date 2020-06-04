#lang scribble/manual

@(require (for-label racket/base
                     racket/contract/base
                     racket/set
                     planning/examples/phase-transition
                     planning/set/action)
          (submod planning/private doc)
          scribble/example)

@(define make-evaluator
   (make-module-sharing-evaluator-factory
    #:public (list 'planning/examples/phase-transition
                   'planning/set/action
                   'racket/set)
    #:private (list 'racket/base)))

@title{Phase Transitions}
@defmodule[planning/examples/phase-transition]

In chemistry, a phase transition is a change from one state of matter to
another. Matter can exist in multiple phases --- consider a glass of water with
an ice cube in it. Therefore, phase transitions can be modeled as
@tech{set actions} that act on the @reference-tech{set} of phases that a
collection of matter is currently in. This is not practically useful on its own,
but it serves as an illustrative example of how to use the
@tech{set state representation}.

@defproc[(state-of-matter? [v any/c]) boolean?]{
 A predicate for the states of matter. Returns true if @racket[v] is one of the
 constants representing the three states of matter: @racket[solid],
 @racket[liquid], and @racket[gas].}

@deftogether[[
 @defthing[solid state-of-matter?]
 @defthing[liquid state-of-matter?]
 @defthing[gas state-of-matter?]]]{
 Three basic states of matter. Other, more exotic states of matter exist, but we
 omit them to keep the examples simple.}

@deftogether[[
 @defthing[melt set-action?]
 @defthing[freeze set-action?]
 @defthing[evaporate set-action?]
 @defthing[condense set-action?]
 @defthing[sublime set-action?]
 @defthing[deposit set-action?]]]{
 Total phase transitions between the three basic states of matter. Each
 transition requires some matter to exist in a specific state, then once it
 is applied that matter is transformed into a different state.

 @(examples
   #:eval (make-evaluator) #:once
   (set-act (set liquid) freeze)
   (eval:error (set-act (set solid) evaporate)))}

@deftogether[[
 @defthing[partially-melt set-action?]
 @defthing[partially-freeze set-action?]
 @defthing[partially-evaporate set-action?]
 @defthing[partially-condense set-action?]
 @defthing[partially-sublime set-action?]
 @defthing[partially-deposit set-action?]]]{
 Partial phase transitions between the basic states of matter, in which some ---
 but not all --- of the matter is changed from one phase to another.

 @(examples
   #:eval (make-evaluator) #:once
   (set-act (set solid) partially-melt))}
