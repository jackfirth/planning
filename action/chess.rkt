#lang racket

(require chess
         planning/action/hash
         rebellion/collection/multidict)

;@------------------------------------------------------------------------------

(define (chess-rank+ rank amount)
  (chess-rank (+ (chess-rank-index rank) amount)))

(define (white-pawn-move start)
  (define end
    (chess-square #:rank (chess-rank+ (chess-square-rank start) 1)
                  #:file (chess-square-file start)))
  (jump-move white-pawn start end))

(define (black-pawn-move start)
  (define end
    (chess-square #:rank (chess-rank+ (chess-square-rank start) -1)
                  #:file (chess-square-file start)))
  (jump-move black-pawn start end))

(define (jump-move piece start end)
  (hash-action #:requirements (multidict start piece)
               #:obstructing-keys (set end)
               #:additions (hash end piece)
               #:deletions (set start)))

(define num-pawn-moves (* 8 5 2))
(define num-double-pawn-moves (* 8 2))
(define num-pawn-captures (* 7 5 2 2))
(define num-pawn-promotions (* 8 4 2))
(define num-pawn-capturing-promotions (* 7 4 2 2))

(define num-pawn-actions
  (+ num-pawn-moves
     num-double-pawn-moves
     num-pawn-captures
     num-pawn-promotions
     num-pawn-capturing-promotions))

(define num-king-moves
  (+ (* 8 6 6 2 2)
     (* 3 4 2 2)
     (* 5 6 4 2 2)))

(hash-action #:requirements (multidict a7 black-pawn)
             #:obstructing-keys (set a6)
             #:additions (hash a6 black-pawn)
             #:deletions (set a7))
(hash-action #:requirements (multidict a2 white-pawn
                                       b3 black-pawn
                                       b3 black-knight
                                       b3 black-bishop
                                       b3 black-rook
                                       b3 black-king
                                       b3 black-queen)
             #:additions (hash b3 white-pawn)
             #:deletions (set a3))
