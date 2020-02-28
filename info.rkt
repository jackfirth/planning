#lang info

(define collection "planning")

(define scribblings
  (list (list "main.scrbl"
              (list 'multi-page)
              (list 'library)
              "planning")))

(define deps
  (list "rebellion"
        "base"))

(define build-deps
  (list "racket-doc"
        "rackunit-lib"
        "scribble-lib"))
