#lang scribble/manual

@(require (for-label planning/multiset/action
                     planning/multiset/goal
                     planning/multiset/problem
                     racket/base
                     racket/contract/base
                     racket/math
                     racket/set
                     rebellion/base/option
                     rebellion/base/range
                     rebellion/collection/hash
                     rebellion/collection/multiset)
          (submod planning/private doc)
          scribble/example)

@(define make-evaluator
   (make-module-sharing-evaluator-factory
    #:public (list 'planning/multiset/action
                   'planning/multiset/goal
                   'planning/multiset/problem
                   'racket/set
                   'rebellion/base/option
                   'rebellion/base/range
                   'rebellion/collection/multiset)
    #:private (list 'racket/base)))

@title{The Multiset State Representation}

In the @tech{multiset state representation}, the world is represented by a
@rebellion-tech{multiset}. Actions and goals are represented by
@tech{multiset actions} and @tech{multiset goals}.

@section{Multiset Actions}
@defmodule[planning/multiset/action]

A @deftech{multiset action} is an @tech{action} on @rebellion-tech{multisets}.
Multiset actions have four components:

@itemlist[
 @item{A hash of preconditions, where each key is an element and the
  corresponding value is a @rebellion-tech{range} of natural numbers describing
  how many copies of the element must be in the multiset for the action to be
  @tech{applicable}.}

 @item{A collection of elements to remove from the multiset. Attempting to
  remove elements that a multiset does not contain is allowed, but it has no
  effect.}

 @item{A collection of elements to add to the multiset.}

 @item{A hash of replacements, where each key is an element and the
  corresponding value is a natural number that determines how many copies of the
  element the multiset will contain after the action is performed.}]

@defproc[(multiset-action? [v any/c]) boolean?]{
 A predicate for @tech{multiset actions}.}

@defproc[(multiset-action
          [#:preconditions preconditions (hash/c any/c range?) empty-hash]
          [#:deletions deletions multiset? empty-multiset]
          [#:additions additions multiset? empty-multiset]
          [#:replacements replacements (hash/c any/c natural?)
           empty-hash]
          [#:cost cost (>=/c 0) 1])
         multiset-action?]{
 Constructs a @tech{multiset action}.

 @(examples
   #:eval (make-evaluator)
   (eval:no-prompt
    (define make-water
      (multiset-action
       #:preconditions (hash 'hydrogen (at-least-range 2)
                             'oxygen (at-least-range 1))
       #:additions (multiset 'water)
       #:deletions (multiset 'hydrogen 'hydrogen 'oxygen))))
   (multiset-act (multiset 'hydrogen 'hydrogen 'oxygen 'carbon) make-water))}

@defproc[(multiset-act [set multiset?] [action multiset-action?]) multiset?]{
 Performs @racket[action] on @racket[set], returning a new multiset. The action
 must be @tech{applicable} to @racket[set] or else a contract error is raised.}

@defproc[(multiset-action-applicable? [action multiset-action?] [set multiset?])
         boolean?]{
 Determines whether @racket[action] is applicable to @racket[set], based on
 whether the preconditions of @racket[action] are satisfied.

 @(examples
   #:eval (make-evaluator)
   (define red-to-blue
     (multiset-action
      #:preconditions (hash 'red (at-least-range 1))
      #:additions (multiset 'blue)
      #:deletions (multiset 'red)))
   (multiset-action-applicable? red-to-blue (multiset 'red))
   (multiset-action-applicable? red-to-blue (multiset 'green)))}

@section{Multiset Goals}
@defmodule[planning/multiset/goal]

A @deftech{multiset goal} is a @tech{goal} in the
@tech{multiset state representation}. Multiset goals contain only a hash of
preconditions of the same form as the preconditions in a @tech{multiset action}.

@defproc[(multiset-goal? [v any/c]) boolean?]{
 A predicate for @tech{multiset goals}.}

@defproc[(multiset-goal [preconditions (hash/c any/c range?)]) multiset-goal?]{
 Constructs a @tech{multiset goal}.}

@section{Multiset Planning Problems}
@defmodule[planning/multiset/problem]

A @deftech{multiset planning problem} is a combination of a
@rebellion-tech{multiset}, a set of @tech{multiset actions}, and a
@tech{multiset goal}. A solution to the problem is a list of actions to perform
that will transform the multiset into a multiset that satisfies the goal.

@defproc[(multiset-planning-problem? [v any/c]) boolean?]{
 A predicate for @tech{multiset planning problems}.}

@defproc[(multiset-planning-problem
          [#:state state multiset?]
          [#:actions actions (set/c multiset-action?)]
          [#:goal goal multiset-goal?])
         multiset-planning-problem?]{
 Constructs a @tech{multiset planning problem}.}

@defproc[(multiset-planning-problem-state [problem multiset-planning-problem?])
         multiset?]{
 Returns the @rebellion-tech{multiset} representing the initial state in
 @racket[problem].}

@defproc[(multiset-planning-problem-actions
          [problem multiset-planning-problem?])
         (set/c multiset-action?)]{
 Returns the @tech{multiset actions} that may be performed as part of a plan for
 @racket[problem].}

@defproc[(multiset-planning-problem-goal [problem multiset-planning-problem?])
         multiset-goal?]{
 Returns the @tech{multiset goal} that a plan for @racket[problem] must
 achieve.}

@defproc[(multiset-plan [problem multiset-planning-problem?])
         (option/c (listof multiset-action?))]{
 Attempts to solve @racket[problem] and return a plan in the form of a list of
 actions to take to achieve the problem's goal. A solution may not exist. The
 current planner is not implemented efficiently, so it is possible to construct
 multiset problems that take too long to solve or for which the planner fails to
 terminate.

 @(examples
   #:eval (make-evaluator)
   (eval:no-prompt
    (define destroy-water
      (multiset-action
       #:preconditions (hash 'water (at-least-range 1))
       #:deletions (multiset 'water)
       #:additions (multiset 'hydrogen 'hydrogen 'oxygen)))

    (define create-peroxide
      (multiset-action
       #:preconditions (hash 'hydrogen (at-least-range 2)
                             'oxygen (at-least-range 2))
       #:deletions (multiset 'hydrogen 'hydrogen 'oxygen 'oxygen)
       #:additions (multiset 'peroxide)))

    (define initial-state (multiset 'water 'water))
    
    (define create-peroxide-from-water
      (multiset-planning-problem
       #:state initial-state
       #:actions (set destroy-water create-peroxide)
       #:goal (multiset-goal (hash 'peroxide (singleton-range 1))))))

   (define the-plan (multiset-plan create-peroxide-from-water))
   (multiset-action-perform-all (present-value the-plan) initial-state))}
