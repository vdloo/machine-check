#!/usr/bin/env racket
#lang racket/base
(require rackunit)
(require rackunit/text-ui)
(require racket/file)

(require "../../check-helpers.rkt")

(module+ test
  (define check-tests
    (test-suite
      "Testsuite for machine-check/machine-check.rkt -> check-file-does-not-contain"

      (test-case
        "Test that check-file-does-not-contain fails if file does contain"
        (define file->string-with
          (λ (path) "Some string that does contain the should-contain string"))

        (check-exn exn:fail?
          (λ ()
            (check-file-does-not-contain 
              "/tmp/some-file-somewhere" 
              "does contain the should-contain"
              #:file->string-with file->string-with))))

       (test-case
        "Test that check-file-does-not-contain does not fail if file does not contain"
        (define file->string-with
          (λ (path) "Some string that does not contain the should-contain string"))

          (check-file-does-not-contain 
            "/tmp/some-file-somewhere" 
            "string that is not in that other string"
            #:file->string-with file->string-with))
       ))

(run-tests check-tests))
