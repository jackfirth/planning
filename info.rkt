#lang info

(define collection "planning")

(define scribblings
  (list (list "main.scrbl"
              (list 'multi-page)
              (list 'library)
              "planning")))

(define deps
  (list "snip-lib"
        "draw-lib"
        "gui-lib"
        "pict-lib"
        "slideshow-lib"
        "chess"
        "fancy-app"
        "point-free"
        "rebellion"
        "base"))

(define build-deps
  (list "pict-doc"
        "racket-doc"
        "rackunit-lib"
        "scribble-lib"))
