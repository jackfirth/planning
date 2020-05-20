#lang racket/base

(require pict
         planning/hash/action
         planning/hash/goal
         planning/hash/problem
         planning/private/animation
         racket/match
         racket/set
         rebellion/base/option
         rebellion/collection/entry
         rebellion/collection/hash
         rebellion/collection/list
         rebellion/collection/multidict
         rebellion/streaming/reducer
         rebellion/streaming/transducer
         rebellion/type/enum
         rebellion/type/tuple)

;@------------------------------------------------------------------------------

(define-enum-type object (player block wall))
(define-tuple-type space (row column))

(define (space-above s)
  (match-define (space r c) s)
  (space (add1 r) c))

(define (space-below s)
  (match-define (space r c) s)
  (space (sub1 r) c))

(define (space-to-left s)
  (match-define (space r c) s)
  (space r (sub1 c)))

(define (space-to-right s)
  (match-define (space r c) s)
  (space r (add1 c)))

(define (move start adjacent-space-finder)
  (define destination (adjacent-space-finder start))
  (hash-action
   #:requirements (multidict start player)
   #:obstructing-keys (set destination)
   #:deletions (set start)
   #:additions (hash destination player)))

(define (push start adjacent-space-finder)
  (define destination (adjacent-space-finder start))
  (define resting-place (adjacent-space-finder destination))
  (hash-action
   #:requirements (multidict start player destination block)
   #:obstructing-keys (set resting-place)
   #:deletions (set start)
   #:additions (hash destination player resting-place block)))

(define adjacent-space-finders
  (set space-above space-below space-to-left space-to-right))

(define (block-world-actions rows columns)
  (for*/set ([r (in-range rows)]
             [c (in-range columns)]
             [finder (in-immutable-set adjacent-space-finders)]
             [action-maker (in-list (list move push))])
    (action-maker (space r c) finder)))

(define block-size 50)
(define board-rows 4)
(define board-columns 4)
(define board-width (* board-columns block-size))
(define board-height (* board-rows block-size))

(define player-pict
  (cc-superimpose (blank block-size)
                  (filled-rectangle (* block-size 4/5) (* block-size 4/5)
                                    #:color "white"
                                    #:draw-border? #t
                                    #:border-color "black"
                                    #:border-width 4)
                  (text "P")))

(define block-pict
  (cc-superimpose (blank block-size)
                  (filled-rectangle (* block-size 4/5) (* block-size 4/5)
                                    #:color "white"
                                    #:draw-border? #t
                                    #:border-color "black"
                                    #:border-width 4)
                  (text "B")))

(define (block-world-pict state)
  (define pin-into-blank
    (make-fold-reducer
     (λ (bkg e)
       (match-define (entry (space r c) p) e)
       (define x (* c block-size))
       (define y (+ board-height (* (add1 r) (- block-size))))
       (pin-over bkg x y p))
     (rectangle board-width board-height)))
  (transduce (in-hash-entries state)
             (mapping-values
              (λ (obj)
                (match obj
                  [(== player) player-pict]
                  [(== block) block-pict])))
             #:into pin-into-blank))

(module+ main
  (define world
    (hash (space 0 0) player
          (space 1 1) block))

  (define actions (block-world-actions board-rows board-columns))
  (define goal (hash-goal #:requirements (multidict (space 3 3) block)))

  (define plan
    (hash-plan
     (hash-planning-problem #:state world #:actions actions #:goal goal)))

  ;; TODO(https://github.com/jackfirth/rebellion/issues/346): Skip the
  ;;   intermediate list by inserting the initial state directly into the
  ;;   stream and fusing these two stream pipelines.
  (define states
    (list-insert (transduce (present-value plan)
                            (folding hash-act world)
                            #:into into-list)
                 world))
  (define block-world-plan
    (transduce states (mapping block-world-pict) #:into (into-animation)))
  (convert-to-file block-world-plan 'gif-bytes "block-world-plan.gif"
                   #:exists 'replace))