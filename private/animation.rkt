#lang racket/base

(require racket/contract/base)

(provide
 (contract-out
  [animation
   (->* ((sequence/c pict?)) (#:frame-rate (and/c rational? positive? exact?))
        animation?)]
  [into-animation
   (->* () (#:frame-rate (and/c rational? positive? exact?))
        (reducer/c pict? animation?))]
  [animation? predicate/c]))

(require fancy-app
         (except-in racket/class absent)
         racket/gui/base
         racket/sequence
         rebellion/base/option
         rebellion/collection/vector
         rebellion/streaming/reducer
         rebellion/streaming/transducer
         pict)

;@------------------------------------------------------------------------------

;; Credit to https://gist.github.com/alex-hhh/d6bdc9f9b671876d6726396e3c7b05c9

(define animation-snip-class
  (new
   (class snip-class%
     (super-new)
     (send this set-classname "animation-snip"))))

(send (get-the-snip-class-list) add animation-snip-class)

(define animation-snip%
  (class snip%
    (init-field frames frame-rate)
    (super-new)
    (send this set-snipclass animation-snip-class)

    (define frame-interval-millis (* (/ 1 frame-rate) 1000))
    (define frames-vector (sequence->vector frames))
    (define num-frames (vector-length frames-vector))

    (when (zero? num-frames)
      (raise-argument-error 'animation
                            "nonempty sequence of frame picts"
                            frames))
    
    (define max-width
      (present-value
       (transduce frames-vector (mapping pict-width) #:into (into-max))))

    (define max-height
      (present-value
       (transduce frames-vector (mapping pict-height) #:into (into-max))))

    (define frame-index (box 0))

    (define (on-refresh)
      (set-box! frame-index (modulo (add1 (unbox frame-index)) num-frames))
      (define admin (send this get-admin))
      (when admin
        (send admin needs-update this 0 0 max-width max-height)))
    
    (define timer
      (new timer%
           [interval frame-interval-millis]
           [notify-callback on-refresh]))

    (define/override (copy)
      (new animation-snip%
           [frames frames-vector]
           [frame-rate frame-rate]))
    
    (define/override (get-extent dc x y w h descent space lspace rspace)
      (when w (set-box! w max-width))
      (when h (set-box! h max-height))
      ;; NOTE: technically, for picts we can compute these as well
      (when descent (set-box! descent 0.0))
      (when space (set-box! space 0.0))
      (when lspace (set-box! lspace 0.0))
      (when rspace (set-box! rspace 0.0)))

    (define/override (draw dc x y . unused)
      (define pict (vector-ref frames-vector (unbox frame-index)))
      (define x-offset (* (- max-width (pict-width pict)) 0.5))
      (define y-offset (* (- max-height (pict-height pict)) 0.5))
      (draw-pict pict dc (+ x x-offset) (+ y y-offset)))))

(define (animation frames #:frame-rate [frame-rate 1])
  (new animation-snip% [frames frames] [frame-rate frame-rate]))

(define (animation? v) (is-a? v animation-snip%))

(define (into-animation #:frame-rate [frame-rate 1])
  (reducer-map (into-vector) #:range (animation _ #:frame-rate frame-rate)))
