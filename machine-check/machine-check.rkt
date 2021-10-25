#!/usr/bin/env racket
#lang racket/base

(require racket/include)
(require racket/string)
(require racket/port)
(require racket/function)
(require rackunit)
(require detect-os-release)

(provide machine-check)
(provide get-packages-installed)

(define error-count 0)

(define check-output
  (λ (arguments)
    (define-values (sp out in err)
      (apply (curry subprocess #f #f #f) arguments))
    (subprocess-wait sp)
    (define lines (string-split (port->string out) "\n"))
    (close-input-port out)
    (close-output-port in)
    (close-input-port err)
    lines))

(define get-packages-installed
  (λ (#:detect-os-with [detect-os detect-os]
      #:check-output-with [check-output check-output])
     (let ((os-id (detect-os))
           (arch-command '("/usr/bin/pacman" "-Qq"))
           (debian-like-command '("/usr/bin/dpkg-query" "-f" "'${binary:Package}\n'" "-W"))
           (redhat-like-command '("/usr/bin/rpm" "-qa" "--qf" "'%{NAME}\n'")))

       (check-output
         (cond
           [(equal? "arch" os-id) arch-command]
           [(equal? "debian" os-id) debian-like-command]
           [(equal? "ubuntu" os-id) debian-like-command]
           [(equal? "rhel" os-id) redhat-like-command]
           [(equal? "centos" os-id) redhat-like-command]
           [(equal? "fedora" os-id) redhat-like-command]
           [else (raise-arguments-error 
                   'os-not-supported 
                   (format "Your OS ~a is currently not supported by machine-check" os-id))])))))

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
