#!/usr/bin/env racket
#lang racket/base
(require rackunit)
(require rackunit/text-ui)
(require racket/file)

(require "../../check-helpers.rkt")

(module+ test
  (define check-tests
    (test-suite
      "Testsuite for machine-check/machine-check.rkt -> check-file-contains"

      (test-case
        "Test that check-file-contains does not fail if file does contain"
        (define file->string-with
          (λ (path) "Some string that does contain the should-contain string"))

        (check-file-contains 
          "/tmp/some-file-somewhere" 
          "does contain the should-contain"
          #:file->string-with file->string-with))

       (test-case
        "Test that check-file-contains fails if file does not contain"
        (define file->string-with
          (λ (path) "Some string that does not contain the should-contain string"))

        (check-exn exn:fail?
          (λ ()
            (check-file-contains 
              "/tmp/some-file-somewhere" 
              "string that is not in that other string"
              #:file->string-with file->string-with))))
       ))

(run-tests check-tests))
