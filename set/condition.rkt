#lang racket/base

(require racket/contract/base)

(provide
 (contract-out
  [set-meets-condition? (-> set? set-condition? boolean?)]
  [set-condition
   (->* ()
        (#:requirements set-coercible-sequence/c
         #:obstructions set-coercible-sequence/c)
        set-condition?)]
  [set-condition? predicate/c]
  [set-condition-requirements (-> set-condition? set?)]
  [set-condition-obstructions (-> set-condition? set?)]))

(require planning/private
         racket/set
         rebellion/collection/set
         rebellion/type/record)

(module+ test
  (require (submod "..")
           rackunit))

;@------------------------------------------------------------------------------

(define-record-type set-condition (requirements obstructions)
  #:omit-root-binding)

(define (set-condition #:requirements [requirements empty-set]
                       #:obstructions [obstructions empty-set])
  (constructor:set-condition
   #:requirements (sequence->set requirements)
   #:obstructions (sequence->set obstructions)))

(define (set-meets-condition? set condition)
  (and (set-contains-all? set (set-condition-requirements condition))
       (set-contains-none? set (set-condition-obstructions condition))))

(module+ test
  (test-case "set-meets-condition?"
    (test-case "empty condition should be met by any set"
      (check set-meets-condition? empty-set (set-condition))
      (check set-meets-condition? (set 1 2 3) (set-condition)))

    (test-case "requirements must be present"
      (define contains-123 (set-condition #:requirements (set 1 2 3)))
      (check set-meets-condition? (set 1 2 3) contains-123)
      (check set-meets-condition? (set 1 2 3 4 5) contains-123)
      (check-false (set-meets-condition? empty-set contains-123))
      (check-false (set-meets-condition? (set 1 2) contains-123)))

    (test-case "obstructions cannot be present"
      (define not-contains-123 (set-condition #:obstructions (set 1 2 3)))
      (check set-meets-condition? empty-set not-contains-123)
      (check set-meets-condition? (set 4 5) not-contains-123)
      (check-false (set-meets-condition? (set 2) not-contains-123))
      (check-false (set-meets-condition? (set 2 4 5) not-contains-123)))))
