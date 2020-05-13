#lang scribble/manual

@(require (for-label planning
                     planning/multiset/action
                     planning/multiset/goal
                     planning/multiset/problem
                     racket/base
                     racket/contract/base
                     racket/math
                     racket/set
                     rebellion/base/range
                     rebellion/collection/hash
                     rebellion/collection/multiset
                     rebellion/collection/set
                     rebellion/type/enum)
          (submod planning/private doc)
          scribble/example)

@(define make-evaluator
   (make-module-sharing-evaluator-factory
    #:public (list 'planning/multiset/action
                   'planning/multiset/goal
                   'planning/multiset/problem
                   'racket/set
                   'rebellion/base/range
                   'rebellion/collection/multiset
                   'rebellion/type/enum)
    #:private (list 'racket/base)))

@title{Automated Planning}
@defmodule[planning]

The process of @deftech{planning} consists of deciding which actions to perform
in order to achieve some objective. One of the central products of artificial
intelligence research is the field of @deftech{automated planning} --- computer
algorithms capable of efficiently planning and acting in simplified models of
the real world. Automated planning algorithms have thousands of uses, from
playing board games to piloting spacecraft. The @racketmodname[planning] library
is an attempt to make the tools and techniques of automated planning available
to Racketeers.

As of early 2020 I'm building this in my spare time for fun. It is woefully
incomplete, and I only plan to use it in the context of a few games I happen to
be playing (and making) lately. If you would like to use it for something more
serious, that's great! But please contact me first (at the email address in the
package description) to give me a heads up, otherwise you run the risk of me
breaking your code on a whim because I'm assuming this package has no other
users.

@table-of-contents[]

@section{Overview}

@subsection{Basic Terminology}

All @tech{automated planning} focuses on at least one of the following objects:

@itemlist[
 @item{A @deftech{state}, which is a representation of the state of the world.}

 @item{An @deftech{action}, which is a transformation of a state. Not every
  action can be performed in every state: an action must be @deftech{applicable}
  to a state in order to perform it on that state. Every action has a
  @deftech{cost} --- a nonnegative number indicating how much time, money, or
  some other scarce resource it takes to perform the action. Planning often
  involves minimizing costs.}

 @item{A @deftech{goal}, which is a statement about what kinds of states the
  planning agent should try to achieve.}]

A @deftech{planning problem} is a combination of an initial state, a collection
of actions, and a goal. An @deftech{automated planner} is a computer algorithm
that solves planning problems by producing a @deftech{plan} --- a list of
actions to perform that will change the initial state into a state that achieves
the goal.

@subsection{State Representations}

To plan, one must consider the effects of one's actions on the state of the
world. Therefore, an automated planner is limited by its model of the world
state, called the @deftech{state representation}. Choosing an appropriate state
representation is the first step in solving any planning problem. The
@racketmodname[planning] library ships with built-in support for three different
state representations:

@itemlist[
 @item{The @deftech{set state representation}, where the world state is a
  @reference-tech{set} and actions add and remove elements to and from the set.
  Goals are expressed in terms of elements that must be present or absent. This
  is a good general purpose state representation, but the lack of structure can
  make it awkward to work with.}

 @item{The @deftech{multiset state representation}, where the world state is a
  @rebellion-tech{multiset} and actions add and remove multiple copies of
  elements at a time. Goals are expressed in terms of desired quantities of
  different elements. This representation is highly effective in planning
  domains that involve some sort of resource production or inventory management,
  such as chemical processing or videogame crafting systems.}

 @item{The @deftech{hash table state representation}, where the world state is a
  @reference-tech{hash table} and actions add and remove key-value entries to
  and from the hash table. Goals are expressed in terms of desired and undesired
  keys and entries. This representation is fairly effective at modeling objects
  located at different positions in the world, such as pieces on a game board or
  boxes in a shipping warehouse.}]

If you're familiar with classical automated planning textbooks you might notice
that the @deftech{state variable representation} is conspicuously absent. This
is partly because in real-world code I found collection-oriented state
representations to be easier to work with than variable-oriented
representations, but only partly. Mostly it's because it's more complicated and
I never had much personal interest in it. If you wish to use a state variable
representation, please reach out to me so I can learn more about your use case.

@section{The Multiset State Representation}

In the @tech{multiset state representation}, the world is represented by a
@rebellion-tech{multiset}. Actions and goals are represented by
@tech{multiset actions} and @tech{multiset goals}.

@subsection{Multiset Actions}
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
   (multiset-action-perform make-water
                            (multiset 'hydrogen 'hydrogen 'oxygen 'carbon)))}

@subsection{Multiset Goals}
@defmodule[planning/multiset/goal]

A @deftech{multiset goal} is a @tech{goal} in the
@tech{multiset state representation}. Multiset goals contain only a hash of
preconditions of the same form as the preconditions in a @tech{multiset action}.

@defproc[(multiset-goal? [v any/c]) boolean?]{
 A predicate for @tech{multiset goals}.}

@defproc[(multiset-goal [preconditions (hash/c any/c range?)]) multiset-goal?]{
 Constructs a @tech{multiset goal}.}

@subsection{Multiset Planning Problems}
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
    (define-enum-type matter (water hydrogen oxygen peroxide))

    (define destroy-water
      (multiset-action
       #:preconditions (hash water (at-least-range 1))
       #:deletions (multiset water)
       #:additions (multiset hydrogen hydrogen oxygen)))

    (define create-peroxide
      (multiset-action
       #:preconditions (hash hydrogen (at-least-range 2)
                             oxygen (at-least-range 2))
       #:deletions (multiset hydrogen hydrogen oxygen oxygen)
       #:additions (multiset peroxide)))
    
    (define create-peroxide-from-water
      (multiset-planning-problem
       #:state (multiset water water water)
       #:actions (set destroy-water create-peroxide)
       #:goal (multiset-goal (hash peroxide (singleton-range 1))))))

   (multiset-plan create-peroxide-from-water))}
