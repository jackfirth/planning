#lang racket/base

(require racket/contract/base)

(provide
 (contract-out
  [sokoban-object? predicate/c]
  [player sokoban-object?]
  [player-in-storage sokoban-object?]
  [crate sokoban-object?]
  [crate-in-storage sokoban-object?]
  [wall sokoban-object?]
  [storage-location sokoban-object?]
  ;; TODO(https://github.com/jackfirth/planning/issues/2): Use hash-goal/c here.
  [sokoban-goal hash-goal?]
  [space? predicate/c]
  [space (-> natural? natural? space?)]
  [space-x (-> space? natural?)]
  [space-y (-> space? natural?)]
  [sokoban-possible-actions
   (-> (hash/c space? sokoban-object? #:immutable #t) (set/c hash-action?))]
  [sokoban-pict (-> (hash/c space? sokoban-object? #:immutable #t) pict?)]))

(require (for-syntax racket/base)
         pict
         planning/hash/action
         planning/hash/goal
         planning/hash/problem
         planning/private
         racket/match
         racket/math
         racket/set
         rebellion/base/option
         rebellion/collection/entry
         rebellion/collection/hash
         rebellion/collection/multidict
         rebellion/streaming/reducer
         rebellion/streaming/transducer
         rebellion/type/enum
         rebellion/type/tuple)

(module+ main
  (require (submod "..")
           planning/hash/visualize
           planning/private/animation))

;@------------------------------------------------------------------------------

(define-enum-type sokoban-object
  (player crate wall storage-location player-in-storage crate-in-storage))

(define-tuple-type space (x y))

(define (space-above s)
  (match-define (space x y) s)
  (space x (sub1 y)))

(define (space-below s)
  (match-define (space x y) s)
  (space x (add1 y)))

(define (space-to-left s)
  (match-define (space x y) s)
  (space (sub1 x) y))

(define (space-to-right s)
  (match-define (space x y) s)
  (space (add1 x) y))

(define-enum-type direction (up down left right))

(define directions (set up down left right))

(define (space-neighbor s dir)
  (match dir
    [(== up) (space-above s)]
    [(== down) (space-below s)]
    [(== left) (space-to-left s)]
    [(== right) (space-to-right s)]))

(define (move start dir)
  (define destination (space-neighbor start dir))
  (hash-action
   #:requirements (multidict start player)
   #:obstructing-keys (set destination)
   #:deletions (set start)
   #:additions (hash destination player)))

(define (move-into-storage start dir)
  (define destination (space-neighbor start dir))
  (hash-action
   #:requirements (multidict start player destination storage-location)
   #:deletions (set start)
   #:additions (hash destination player-in-storage)))

(define (move-out-of-storage start dir)
  (define destination (space-neighbor start dir))
  (hash-action
   #:requirements (multidict start player-in-storage)
   #:obstructing-keys (set destination)
   #:additions (hash start storage-location destination player-in-storage)))

(define (move-through-storage start dir)
  (define destination (space-neighbor start dir))
  (hash-action
   #:requirements
   (multidict start player-in-storage destination storage-location)
   #:additions (hash start storage-location destination player-in-storage)))

(define (push start dir)
  (define object-location (space-neighbor start dir))
  (define resting-place (space-neighbor object-location dir))
  (hash-action
   #:requirements (multidict start player object-location crate)
   #:obstructing-keys (set resting-place)
   #:deletions (set start)
   #:additions (hash object-location player resting-place crate)))

(define (push-into-storage start dir)
  (define object-location (space-neighbor start dir))
  (define resting-place (space-neighbor object-location dir))
  (hash-action
   #:requirements
   (multidict start player object-location crate resting-place storage-location)
   #:deletions (set start)
   #:additions (hash object-location player resting-place crate-in-storage)))

(define (push-out-of-storage start dir)
  (define object-location (space-neighbor start dir))
  (define resting-place (space-neighbor object-location dir))
  (hash-action
   #:requirements
   (multidict start player object-location crate-in-storage)
   #:obstructing-keys (set resting-place)
   #:deletions (set start)
   #:additions (hash object-location player-in-storage resting-place crate)))

(define (push-through-storage start dir)
  (define object-location (space-neighbor start dir))
  (define resting-place (space-neighbor object-location dir))
  (hash-action
   #:requirements
   (multidict start player
              object-location crate-in-storage
              resting-place storage-location)
   #:deletions (set start)
   #:additions
   (hash object-location player-in-storage resting-place crate-in-storage)))

(define (push-from-storage start dir)
  (define object-location (space-neighbor start dir))
  (define resting-place (space-neighbor object-location dir))
  (hash-action
   #:requirements (multidict start player-in-storage object-location crate)
   #:obstructing-keys (set resting-place)
   #:additions
   (hash start storage-location object-location player resting-place crate)))

(define (push-from-storage-into-storage start dir)
  (define object-location (space-neighbor start dir))
  (define resting-place (space-neighbor object-location dir))
  (hash-action
   #:requirements
   (multidict start player-in-storage
              object-location crate
              resting-place storage-location)
   #:additions
   (hash start storage-location
         object-location player
         resting-place crate-in-storage)))

(define (push-from-storage-out-of-storage start dir)
  (define object-location (space-neighbor start dir))
  (define resting-place (space-neighbor object-location dir))
  (hash-action
   #:requirements
   (multidict start player-in-storage object-location crate-in-storage)
   #:obstructing-keys (set resting-place)
   #:additions
   (hash start storage-location
         object-location player-in-storage
         resting-place crate)))

(define (push-from-storage-through-storage start dir)
  (define object-location (space-neighbor start dir))
  (define resting-place (space-neighbor object-location dir))
  (hash-action
   #:requirements
   (multidict start player-in-storage
              object-location crate-in-storage
              resting-place storage-location)
   #:additions
   (hash start storage-location
         object-location player-in-storage
         resting-place crate-in-storage)))

(define-enum-type action-type
  (move-action
   move-into-storage-action
   move-out-of-storage-action
   move-through-storage-action
   push-action
   push-into-storage-action
   push-out-of-storage-action
   push-through-storage-action
   push-from-storage-action
   push-from-storage-into-storage-action
   push-from-storage-out-of-storage-action
   push-from-storage-through-storage-action))

(define action-types
  (set move-action
       move-into-storage-action
       move-out-of-storage-action
       move-through-storage-action
       push-action
       push-into-storage-action
       push-out-of-storage-action
       push-through-storage-action
       push-from-storage-action
       push-from-storage-into-storage-action
       push-from-storage-out-of-storage-action
       push-from-storage-through-storage-action))

(define (sokoban-action type start dir)
  (match type
    [(== move-action) (move start dir)]
    [(== move-out-of-storage-action) (move-out-of-storage start dir)]
    [(== move-into-storage-action) (move-into-storage start dir)]
    [(== move-through-storage-action)
     (move-through-storage start dir)]
    [(== push-action) (push start dir)]
    [(== push-into-storage-action) (push-into-storage start dir)]
    [(== push-out-of-storage-action) (push-out-of-storage start dir)]
    [(== push-through-storage-action) (push-through-storage start dir)]
    [(== push-from-storage-action) (push-from-storage start dir)]
    [(== push-from-storage-into-storage-action)
     (push-from-storage-into-storage start dir)]
    [(== push-from-storage-out-of-storage-action)
     (push-from-storage-out-of-storage start dir)]
    [(== push-from-storage-through-storage-action)
     (push-from-storage-through-storage start dir)]))

(define (sokoban-row-count state)
  (define spaces (in-immutable-hash-keys state))
  (define max-row (transduce spaces (mapping space-y) #:into (into-max)))
  (option-case max-row #:present add1 #:absent (λ () 0)))

(define (sokoban-column-count state)
  (define spaces (in-immutable-hash-keys state))
  (define max-column
    (transduce spaces (mapping space-x) #:into (into-max)))
  (option-case max-column #:present add1 #:absent (λ () 0)))

(define (sokoban-possible-actions state)
  (define row-count (sokoban-row-count state))
  (define column-count (sokoban-column-count state))
  (for*/set ([r (in-range row-count)]
             [c (in-range column-count)]
             [s (in-value (space r c))]
             #:unless (hash-contains? state s wall)
             [dir (in-immutable-set directions)]
             [type (in-immutable-set action-types)])
    (sokoban-action type s dir)))

(define sokoban-goal
  (hash-goal #:obstructing-values (set crate)))

(define tile-width 32)

(define-external-pict floor-pict '(lib "planning/examples/sokoban-floor.png"))
(define-external-pict player-pict '(lib "planning/examples/sokoban-player.png"))
(define-external-pict crate-pict '(lib "planning/examples/sokoban-crate.png"))
(define-external-pict wall-pict '(lib "planning/examples/sokoban-wall.png"))

(define-external-pict storage-location-pict
  '(lib "planning/examples/sokoban-storage-location.png"))

(define-external-pict player-in-storage-pict
  '(lib "planning/examples/sokoban-player-in-storage.png"))

(define-external-pict crate-in-storage-pict
  '(lib "planning/examples/sokoban-crate-in-storage.png"))

(define (sokoban-object-pict obj)
  (match obj
    [(== player) player-pict]
    [(== crate) crate-pict]
    [(== wall) wall-pict]
    [(== storage-location) storage-location-pict]
    [(== player-in-storage) player-in-storage-pict]
    [(== crate-in-storage) crate-in-storage-pict]))

(define (sokoban-pict state)
  (define row-count (sokoban-row-count state))
  (define column-count (sokoban-column-count state))
  (define width (* column-count tile-width))
  (define height (* row-count tile-width))
  (for*/fold ([bkg (blank width height)])
             ([x (in-range column-count)]
              [y (in-range row-count)])
    (define s (space x y))
    (define next-pict
      (if (hash-has-key? state s)
          (sokoban-object-pict (hash-ref state s))
          floor-pict))
    (pin-over bkg (* x tile-width) (* y tile-width) next-pict)))

(module test racket/base)

(module+ main
  (define state
    (hash (space 0 0) wall
          (space 0 1) wall
          (space 0 2) wall
          (space 0 3) wall
          (space 0 4) wall
          (space 0 5) wall
          (space 1 5) wall
          (space 2 5) wall
          (space 3 5) wall
          (space 4 5) wall
          (space 5 5) wall
          (space 5 4) wall
          (space 5 3) wall
          (space 5 2) wall
          (space 5 1) wall
          (space 5 0) wall
          (space 4 0) wall
          (space 3 0) wall
          (space 2 0) wall
          (space 1 0) wall
          (space 1 1) player
          (space 2 2) crate
          (space 2 3) wall
          (space 3 3) wall
          (space 4 4) storage-location))

  (define actions (sokoban-possible-actions state))

  (define plan
    (hash-visualize-plan!
     (hash-planning-problem #:state state #:actions actions #:goal sokoban-goal)
     #:draw-state-with sokoban-pict))
  (convert-to-file plan 'gif-bytes "sokoban-plan.gif"
                   #:exists 'replace)
  plan)
