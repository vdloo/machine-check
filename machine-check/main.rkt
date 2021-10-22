#!/usr/bin/env racket
#lang racket/base

(require racket/function)

(require "commandline.rkt")
(require "machine-check.rkt")
(provide main)

(define (main #:parse-with [parse-args parse-args]
              #:machine-check-with [machine-check machine-check])
  (parse-args machine-check))
