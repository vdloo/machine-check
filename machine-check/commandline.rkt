#!/usr/bin/env racket
#lang racket/base
(require racket/cmdline)

(provide parse-args)

(define parse-args 
  (λ (entrypoint)
    (command-line
      #:program "machine-check"
      #:usage-help "\nVerify if this machine is configured correctly according to rackunit tests."
      #:ps "\nRun machine-check to check if this machine's state is as we expect."
        "examples: "
         "$ machine-check # Runs the actual check"
         "$ machine-check --help"
      #:args args
      (begin
	(entrypoint args)))))
