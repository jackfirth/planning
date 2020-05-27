#lang scribble/manual

@(require (for-label planning
                     planning/examples/block-world
                     planning/hash/action
                     planning/hash/goal
                     planning/hash/problem
                     planning/hash/visualize
                     planning/multiset/action
                     planning/multiset/goal
                     planning/multiset/problem
                     racket/base
                     racket/contract/base
                     racket/math
                     racket/pretty
                     racket/set
                     rebellion/base/option
                     rebellion/base/range
                     rebellion/collection/hash
                     rebellion/collection/list
                     rebellion/collection/multidict
                     rebellion/collection/multiset
                     rebellion/collection/set
                     rebellion/type/enum)
          (submod planning/private doc)
          scribble/example)

@(define make-evaluator
   (make-module-sharing-evaluator-factory
    #:public (list 'planning/examples/block-world
                   'planning/hash/action
                   'planning/hash/goal
                   'planning/hash/problem
                   '(submod planning/hash/visualize headless)
                   'planning/multiset/action
                   'planning/multiset/goal
                   'planning/multiset/problem
                   'racket/pretty
                   'racket/set
                   'rebellion/base/option
                   'rebellion/base/range
                   'rebellion/collection/list
                   'rebellion/collection/multidict
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

@include-section[(lib "planning/overview.scrbl")]
@include-section[(lib "planning/multiset.scrbl")]
@include-section[(lib "planning/hash.scrbl")]

@section{The Block World}
@defmodule[planning/examples/block-world]

@(examples
  #:eval (make-evaluator) #:once
  (eval:no-prompt
   (define world
     (hash (space 2 1) player
           (space 1 2) block))
  (define goal
    (hash-goal #:requirements (multidict (space 3 3) block (space 2 1) player)))
  (define problem
    (hash-planning-problem
     #:state world #:actions (block-world-actions 4 4) #:goal goal)))

  (hash-visualize-plan! problem #:draw-state-with block-world-pict))
