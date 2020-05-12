#lang scribble/manual

@(require (for-label planning)
          (submod planning/private doc))

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

@table-of-contents[]

@section{Basic Terminology}

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

@section{State Representations}

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
  elements at a time. Goals are expressed in desired quantities of different
  elements. This representation is highly effective in planning domains that
  involve some sort of resource and inventory management, such as chemical
  processing or videogame crafting systems.}

 @item{The @deftech{hash table state representation}, where the world state is a
  @reference-tech{hash table} and actions add and remove key-value entries to
  and from the hash table. Goals are expressed in terms of desired or undesired
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

@include-section[(lib "planning/action.scrbl")]
