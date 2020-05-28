#lang scribble/manual

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

@include-section[(lib "planning/overview.scrbl")]
@include-section[(lib "planning/multiset.scrbl")]
@include-section[(lib "planning/hash.scrbl")]
@include-section[(lib "planning/examples/sokoban.scrbl")]
