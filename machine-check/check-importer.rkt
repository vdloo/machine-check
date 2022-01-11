#!/usr/bin/env racket
#lang racket/base

(require racket/string)
(require reprovide/require-transformer/glob-in)
(require (glob-in "../checks-to-perform/*.rkt"))

(provide perform-checks)

(define get-module-names
  (λ ()
     (map (λ (p) (string-trim p ".rkt"))
       (filter (λ (p) (string-suffix? p ".rkt"))
         (map
           path->string 
           (directory-list "checks-to-perform"))))))


(define perform-checks
  (λ ()
     (map 
       ; See this stackoverflow comment about evaluating
       ; strings to procedures in Racket. Note that this
       ; is REALLY dangerous and shouldn't be done in any
       ; production grade code. But this is Lisp after all
       ; and I want to treat code as data and data as code.
       ; https://stackoverflow.com/a/29461124/4158804
       (λ
         (s) 
         ((eval 
            (string->symbol s) 
            (variable-reference->namespace 
              (#%variable-reference)))))
       (map 
         (λ (n) (format "perform-~a-checks" n))
         (get-module-names)))
     (void)))
