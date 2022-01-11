#!/usr/bin/env racket
#lang racket/base
(require rackunit)
(require rackunit/text-ui)
(require racket/file)

(require "../../check-helpers.rkt")

(module+ test
  (define check-tests
    (test-suite
      "Testsuite for machine-check/machine-check.rkt -> check-file-mode"

      (test-case
        "Test that check-file-mode calls file-or-directory-type with path and false for no must exist"
        (define mock-file-or-directory-type-with 
          (λ (path mustexist) 
             (check-equal? path "/tmp/some-file-somewhere")
             (check-equal? mustexist #f)
             #t))
        (define mock-file-or-directory-permissions-with (λ (path mode) 493))

        (check-file-mode "/tmp/some-file-somewhere" 493
          #:file-or-directory-type-with mock-file-or-directory-type-with
          #:file-or-directory-permissions-with mock-file-or-directory-permissions-with))

       (test-case
        "Test that check-file-mode calls file-or-directory-permissions with path"
        (define mock-file-or-directory-type-with (λ (path mustexist) #t))
        (define mock-file-or-directory-permissions-with 
          (λ (path mode) 
             (check-equal? path "/tmp/some-file-somewhere")
             (check-equal? mode 'bits)
             493))

        (check-file-mode "/tmp/some-file-somewhere" 493
          #:file-or-directory-type-with mock-file-or-directory-type-with
          #:file-or-directory-permissions-with mock-file-or-directory-permissions-with))

       (test-case
        "Test that check-file-mode raises if no such file"
        (define mock-file-or-directory-type-with (λ (path mustexist) #f))

        (check-exn exn:fail?
          (λ ()
          (check-file-mode "/tmp/some-file-somewhere" 493
            #:file-or-directory-type-with mock-file-or-directory-type-with))))
       ))
(run-tests check-tests))
