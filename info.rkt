#lang info
(define collection "unified-for")
(define deps
  '("base"))
(define build-deps
  '("scribble-lib"
    "racket-doc"
    "rackunit-lib"
    "expect"
    "sandbox-lib"))
(define scribblings '(("scribblings/unified-for.scrbl" ())))
(define pkg-desc
  "Consolidates the various flavors of `for` iteration into a single unified for macro.")
(define version "0.0")
(define pkg-authors '(michaelmmacleod))
