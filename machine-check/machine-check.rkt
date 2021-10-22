#!/usr/bin/env racket
#lang racket/base

(require racket/include)
(require racket/string)
(require racket/port)
(require rackunit)

(provide machine-check)
(provide get-packages-installed)

(define error-count 0)

(define check-output
  (λ (command argument)
    (define-values (sp out in err)
      (subprocess #f #f #f command argument))
    (subprocess-wait sp)
    (define lines (string-split (port->string out) "\n"))
    (close-input-port out)
    (close-output-port in)
    (close-input-port err)
    lines))

(define get-packages-installed
  (λ (#:check-output-with [check-output check-output])
     (check-output "/usr/bin/pacman" "-Qq")))

(define check-package-installed
  (λ (package-name)
     (check-false (not (member package-name (get-packages-installed)))
         (format "Package '~a' was not found installed" package-name))))

(define check-packages-installed
  (λ (package-names)
     (for-each
       check-package-installed
       package-names)))

(define perform-checks
  (λ () (include (file "/srv/machine-check/main.rkt"))))

(define machine-check
  (λ (args)
     (begin
       (define old-around (current-check-around))
       (define (count-errors thunk)
         (with-handlers ([exn:test:check? (λ (e) (set! error-count (+ 1 error-count))
                                            (old-around thunk))])
           (thunk)))
       (parameterize ([current-check-around count-errors])
       (perform-checks)
         (exit (min error-count 1))))))
