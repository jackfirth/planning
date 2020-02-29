#lang racket/base

(require racket/contract/base)

(provide
 (contract-out
  [set-contains-all? (-> set? set? boolean?)]
  [set-contains-none? (-> set? set? boolean?)]))

(module+ doc
  (provide
   (contract-out
    [racket-reference-tech (-> pre-content? ... element?)])))

(require racket/set)

(module+ doc
  (require scribble/core
           scribble/decode
           scribble/manual))

;@------------------------------------------------------------------------------

(define (set-contains-all? s items)
  (subset? items s))

(define (set-contains-none? s items)
  (equal? (set-count (set-subtract s items)) (set-count s)))

(module+ doc
  (define (racket-reference-tech . text)
    (apply tech #:doc '(lib "scribblings/reference/reference.scrbl") text)))
