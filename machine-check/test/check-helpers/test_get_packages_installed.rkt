#!/usr/bin/env racket
#lang racket/base
(require rackunit)
(require rackunit/text-ui)

(require "../../check-helpers.rkt")

(module+ test
  (define check-tests
    (test-suite
      "Testsuite for machine-check/machine-check.rkt -> get-packages-installed"

      (test-case
        "Test that get-packages-installed calls check-output with pacman on Arch Linux"
        (define mock-detect-os-with
          (λ ()
             "arch"))
        (define mock-check-output 
          (λ (arguments)
             (check-equal? arguments '("/usr/bin/pacman" "-Qq"))))

        (get-packages-installed 
          #:detect-os-with mock-detect-os-with
          #:check-output-with mock-check-output))

      (test-case
        "Test that get-packages-installed calls check-output with apt on Debian"
        (define mock-detect-os-with
          (λ ()
             "debian"))
        (define mock-check-output 
          (λ (arguments)
             (check-equal? arguments '("/usr/bin/dpkg-query" "-f" "${binary:Package}\n" "-W"))))

        (get-packages-installed 
          #:detect-os-with mock-detect-os-with
          #:check-output-with mock-check-output))

      (test-case
        "Test that get-packages-installed calls check-output with apt on Ubuntu"
        (define mock-detect-os-with
          (λ ()
             "ubuntu"))
        (define mock-check-output 
          (λ (arguments)
             (check-equal? arguments '("/usr/bin/dpkg-query" "-f" "${binary:Package}\n" "-W"))))

        (get-packages-installed 
          #:detect-os-with mock-detect-os-with
          #:check-output-with mock-check-output))

      (test-case
        "Test that get-packages-installed calls check-output with rpm on CentOS"
        (define mock-detect-os-with
          (λ ()
             "centos"))
        (define mock-check-output 
          (λ (arguments)
             (check-equal? arguments '("/usr/bin/rpm" "-qa" "--qf" "'%{NAME}\n'"))))

        (get-packages-installed 
          #:detect-os-with mock-detect-os-with
          #:check-output-with mock-check-output))

      (test-case
        "Test that get-packages-installed calls check-output with rpm on Red Hat"
        (define mock-detect-os-with
          (λ ()
             "rhel"))
        (define mock-check-output 
          (λ (arguments)
             (check-equal? arguments '("/usr/bin/rpm" "-qa" "--qf" "'%{NAME}\n'"))))

        (get-packages-installed 
          #:detect-os-with mock-detect-os-with
          #:check-output-with mock-check-output))

      (test-case
        "Test that get-packages-installed calls check-output with apt on Fedora"
        (define mock-detect-os-with
          (λ ()
             "fedora"))
        (define mock-check-output 
          (λ (arguments)
             (check-equal? arguments '("/usr/bin/rpm" "-qa" "--qf" "'%{NAME}\n'"))))

        (get-packages-installed 
          #:detect-os-with mock-detect-os-with
          #:check-output-with mock-check-output))

      (test-case
        "Test that get-packages-installed raises if unsupported distro"
        (define mock-detect-os-with
          (λ ()
             "gentoo"))
        (define mock-check-output 
          (λ (arguments)
             (check-true #f)))  ; check-output should not be called in this test-case

        (check-exn exn:fail:contract?
          (λ ()
            (get-packages-installed 
              #:detect-os-with mock-detect-os-with
              #:check-output-with mock-check-output))))

      ))

(run-tests check-tests))
