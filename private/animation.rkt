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
  [animation? predicate/c]
  ;; TODO: Move this to another module. There isn't really a good reason to put
  ;; convert-to-file in the same module as animations, it just happened to be
  ;; easy to do.
  [convert-to-file
   (->* (convertible? interned-symbol? path-string?)
        (#:exists interned-symbol?) ;; TODO: Use a better contract here.
        void?)]))

(require fancy-app
         file/convertible
         file/gif
         (except-in racket/class absent)
         (only-in racket/gui/base get-the-snip-class-list timer%)
         racket/math
         racket/sequence
         racket/snip
         rebellion/base/option
         rebellion/base/symbol
         rebellion/collection/vector
         rebellion/streaming/reducer
         rebellion/streaming/transducer
         pict)

;@------------------------------------------------------------------------------

;; Credit to https://gist.github.com/alex-hhh/d6bdc9f9b671876d6726396e3c7b05c9

(define convertible<%>
  (interface*
   ()
   ([prop:convertible (λ (v request default) (send v convert request default))])
   convert))

(define animation-snip-class
  (new
   (class snip-class%
     (super-new)
     (send this set-classname "animation-snip"))))

(send (get-the-snip-class-list) add animation-snip-class)

(define animation-snip%
  (class* snip% (convertible<%>)
    (init-field frames frame-rate)
    (super-new)
    (send this set-snipclass animation-snip-class)

    (define frame-interval-seconds (/ 1 frame-rate))
    (define frame-interval-milliseconds (* frame-interval-seconds 1000))
    (define frame-interval-centiseconds (* frame-interval-seconds 100))
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
           [interval frame-interval-milliseconds]
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
      (draw-pict pict dc (+ x x-offset) (+ y y-offset)))

    (define (frame-top-left-point-x frame)
      (/ (- max-width (pict-width frame)) 2))

    (define (frame-bottom-right-point-x frame)
      (+ (frame-top-left-point-x frame) (pict-width frame)))

    (define (frame-top-left-point-y frame)
      (/ (- max-height (pict-height frame)) 2))

    (define (frame-bottom-right-point-y frame)
      (+ (frame-top-left-point-y frame) (pict-height frame)))

    (define (convert-to-gif-bytes)
      ;; The GIF file format defines the "dimension" type as an integer between
      ;; 0 and 65535 inclusive, and this type is used for various encoded fields
      ;; in control messages. One of these fields is the amount of time (in
      ;; centiseconds) between images. As a result, extremely slow animations
      ;; are impossible to render as GIFs without inserting duplicate frames to
      ;; ensure the delay between frames is never greater than 655.35 seconds.
      ;; Instead of bothering with this, we just fail fast with an error message
      ;; that explains the situation.
      (unless (< frame-interval-centiseconds 65535)
        (raise-arguments-error
         'animation-convert
         "This animation cannot be converted to a GIF because its frame rate \
is too slow. The GIF file format does not support animations with more than \
655 seconds between frames. Either increase the frame rate, insert duplicate \
frames, or choose a different file format."
         "animation frame rate" frame-rate
         "seconds between frames" (exact-floor frame-interval-seconds)
         "number of frames" num-frames))
      
      (define converted-bytes (open-output-bytes 'rendered-gif-bytes))
      (define gifstream (gif-start converted-bytes max-width max-height 0 #f))
      (gif-add-loop-endlessly-control gifstream)
      (for/fold ([first-frame? #t] #:result (void))
                ([frame (in-vector frames-vector)])
        (define x (frame-top-left-point-x frame))
        (define y (frame-top-left-point-y frame))
        (define frame-delay (if first-frame? 0 frame-interval-centiseconds))
        (gif-add-control gifstream 'restore-bg #f frame-delay #f)
        (gif-add-pict gifstream x y frame)
        #f)
      (gif-end gifstream)
      (bytes->immutable-bytes (get-output-bytes converted-bytes)))

    (define/public (convert request default)
      (case request
        [(gif-bytes) (convert-to-gif-bytes)]
        [else default]))))

(define (animation frames #:frame-rate [frame-rate 1])
  (new animation-snip% [frames frames] [frame-rate frame-rate]))

(define (animation? v) (is-a? v animation-snip%))

(define (into-animation #:frame-rate [frame-rate 1])
  (reducer-map (into-vector) #:range (animation _ #:frame-rate frame-rate)))

(define (gif-add-pict gifstream x y p)
  (define w (pict-width p))
  (define h (pict-height p))
  (define pixels (pict->argb-pixels p))
  (define-values (bytes colormap transparent) (quantize pixels))
  (gif-add-image gifstream x y w h #f colormap bytes))

(define (gif-add-loop-endlessly-control gifstream)
  ;; The second argument is the number of times the gif's images should be
  ;; looped, but zero means the images should be looped forever.
  (gif-add-loop-control gifstream 0))

(define (convert-to-file v request destination #:exists [mode 'error])
  (define file-port (open-output-file destination #:exists mode))
  (void (write-bytes (convert v request) file-port))
  (close-output-port file-port))
