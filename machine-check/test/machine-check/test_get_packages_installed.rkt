#!/usr/bin/env racket
#lang racket/base
(require rackunit)
(require rackunit/text-ui)

(require "../../machine-check.rkt")

(module+ test
  (define check-tests
    (test-suite
      "Testsuite for machine-check/machine-check.rkt -> get-packages-installed"
      (test-case
        "Test that get-packages-installed calls check-output with pacman as argument"
        (define mock-check-output 
	  (λ (command argument)
	     (check-equal? command "/usr/bin/pacman")
	     (check-equal? argument "-Qq")))

	(get-packages-installed #:check-output-with mock-check-output))
      ))

(run-tests check-tests))
