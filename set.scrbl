#lang scribble/manual

@(require (for-label planning/set/action
                     planning/set/condition
                     racket/base
                     racket/contract/base
                     racket/sequence
                     racket/set
                     rebellion/collection/set)
          (submod planning/private doc)
          scribble/example)

@(define make-evaluator
   (make-module-sharing-evaluator-factory
    #:public (list 'planning/set/action
                   'planning/set/condition
                   'racket/set)
    #:private (list 'racket/base)))

@title{The Set State Representation}

In the @tech{set state representation}, the world is represented by a
@reference-tech{set}. Actions and goals are represented by
@tech{set actions} and @tech{set conditions}.

@section{Set Actions}
@defmodule[planning/set/action]

A @deftech{set action} is an @tech{action} in the
@tech{set state representation}.

@defproc[(set-action? [v any/c]) boolean?]{
 A predicate for @tech{set actions}.}

@section{Set Conditions}
@defmodule[planning/set/condition]

A @deftech{set condition} is a @tech{condition} in the
@tech{set state representation}. A set condition is a set of requirements and a
set of obstructions. To meet a set condition, a set must contain all of the
condition's requirements and none of its obstructions.

@defproc[(set-condition? [v any/c]) boolean?]{
 A predicate for @tech{set conditions}.}

@defproc[(set-condition
          [#:requirements requirements (sequence/c any/c) empty-set]
          [#:obstructions obstructions (sequence/c any/c) empty-set])
         set-condition?]{
 Constructs a @tech{set condition}.}

@defproc[(set-meets-condition? [s set?] [condition set-condition?]) boolean?]{
 Determines whether or not @racket[s] meets @racket[condition]. To meet
 @racket[condition], @racket[s] must contain all of the condition's requirements
 and none of its obstructions.

 @(examples
   #:eval (make-evaluator) #:once
   (define primes (set 2 3 5 7 11))
   (set-meets-condition? primes (set-condition #:requirements (set 7 11)))
   (set-meets-condition? primes (set-condition #:obstructions (set 2 4 6 8)))
   (set-meets-condition? primes (set-condition #:obstructions (set 4 6 8))))}

@defproc[(set-condition-requirements [condition set-condition?]) set?]{
 Returns the requirements of @racket[condition].}

@defproc[(set-condition-obstructions [condition set-condition?]) set?]{
 Returns the obstructions of @racket[condition].}
