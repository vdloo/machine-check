#!/usr/bin/env racket
#lang racket/base

(require racket/include)
(require rackunit)

(require "check-importer.rkt")

(provide machine-check)

(define error-count 0)

(define machine-check
  (λ (args)
     (begin
       (define old-around (current-check-around))
       (define (count-errors thunk)
         (with-handlers
           ([exn:test:check?
              (λ (e)
               (set! error-count (+ 1 error-count))
               (old-around thunk))])
           (thunk)))
       (parameterize ([current-check-around count-errors])
         (perform-checks)
       (if (eq? error-count 0) (displayln "\nAll tests pass!") '())
       (exit (min error-count 1))))))
