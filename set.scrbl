#lang scribble/manual

@(require (for-label planning/examples/phase-transition
                     planning/set/action
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
    #:public (list 'planning/examples/phase-transition
                   'planning/set/action
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

@defproc[(set-action
          [#:requirements requirements (sequence/c any/c) empty-set]
          [#:obstructions obstructions (sequence/c any/c) empty-set]
          [#:additions additions (sequence/c any/c) empty-set]
          [#:deletions deletions (sequence/c any/c) empty-set]
          [#:cost cost (>=/c 0) 1])
         set-action?]{
 Constructs a @tech{set action}.}

@defproc[(set-act [s set?] [action set-action?]) set?]{
 Performs @racket[action] on @racket[s]. If @racket[action] is not
 @tech{applicable} in @racket[s], a contract error is raised.

 @(examples
   #:eval (make-evaluator) #:once
   (set-act (set 1 2 3)
            (set-action #:additions (set 10) #:deletions (set 2 3)))
   (eval:error
    (set-act (set 1 2 3)
             (set-action #:requirements (set 5) #:additions (set 10)))))}

@defproc[(set-action-applicable? [action set-action?] [s set?]) boolean?]{
 Determines whether or not @racket[action] is @tech{applicable} in @racket[s]:
 that is, whether or not @racket[s] meets the action's preconditions.}

@defproc[(set-action-requirements [action set-action?]) set?]{
 Returns the requirements of @racket[action].}

@defproc[(set-action-obstructions [action set-action?]) set?]{
 Returns the obstructions of @racket[action].}

@defproc[(set-action-additions [action set-action?]) set?]{
 Returns the additions of @racket[action].}

@defproc[(set-action-deletions [action set-action?]) set?]{
 Returns the deletions of @racket[action].}

@defproc[(set-action-cost [action set-action?]) (>=/c 0)]{
 Returns the cost of @racket[action].}

@defproc[(set-action-preconditions [action set-action?]) set-condition?]{
 Returns a @tech{set condition} describing the @tech{preconditions} of
 @racket[action].}

@defproc[(set-action-postconditions [action set-action?]) set-condition?]{
 Returns a @tech{set condition} describing the @tech{postconditions} of
 @racket[action].

 @(examples
   #:eval (make-evaluator) #:once
   (eval:no-prompt
    (define action
      (set-action
       #:requirements (set 1 3 5)
       #:additions (set 2 4)
       #:deletions (set 5))))
   
   (set-action-preconditions action)
   (set-action-postconditions action))}

@defproc[(set-invariants [s set?] [actions (sequence/c set-action?)])
         set-condition?]{
 Determines the invariants of @racket[s] --- that is, the conditions that will
 always be true no matter what sequence of actions from @racket[actions] are
 performed.

 @(examples
   #:eval (make-evaluator) #:once
   (set-invariants (set liquid) (set melt freeze))
   (set-invariants (set solid liquid) (set evaporate condense)))}

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
