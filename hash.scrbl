#lang scribble/manual

@(require (for-label planning/hash/action
                     planning/hash/goal
                     planning/hash/problem
                     racket/base
                     racket/contract/base)
          (submod planning/private doc)
          scribble/example)

@(define make-evaluator
   (make-module-sharing-evaluator-factory
    #:public (list 'planning/hash/action
                   'planning/hash/goal
                   'planning/hash/problem)
    #:private (list 'racket/base)))

@title{The Hash State Representation}

In the @tech{hash table state representation}, the world is represented by a
@reference-tech{hash table}. Actions and goals are represented by
@tech{hash actions} and @tech{hash goals}.

@section{Hash Actions}
@defmodule[planning/hash/action]

A @deftech{hash action} is an @tech{action} on @reference-tech{hash tables}.

@defproc[(hash-action? [v any/c]) boolean?]{
 A predicate for @tech{hash actions}.}

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
