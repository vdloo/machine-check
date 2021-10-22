#!/usr/bin/env racket
#lang racket/base
(require rackunit)
(require rackunit/text-ui)

(require "../main.rkt")

(module+ test
  (define main-tests
    (test-suite
      "Testsuite for machine-check/main.rkt -> main"
      (test-case
        "Test that main calls parse-args with check as the argument"
        (define mock-machine-check 'mock-machine-check)
        (define mock-parse-with (λ (arg) (check-equal? arg mock-machine-check)))

        (main #:parse-with mock-parse-with
              #:machine-check-with mock-machine-check))
      ))

(run-tests main-tests))
