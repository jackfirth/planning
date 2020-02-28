#lang racket/base

(require racket/contract/base)

(provide
 (contract-out
  [set-contains-all? (-> set? set? boolean?)]
  [set-contains-none? (-> set? set? boolean?)]))

(require racket/set)

;@------------------------------------------------------------------------------

(define (set-contains-all? s items)
  (subset? items s))

(define (set-contains-none? s items)
  (equal? (set-count (set-subtract s items)) (set-count s)))
