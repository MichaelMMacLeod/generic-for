#lang racket/base

;;;
;;; make-iterator.rkt
;;;
;;; Provides make-iterator, a function which can be used to aid the creation of iterators.
;;;

(provide (all-defined-out))

(define (make-iterator
         #:outside-bindings [outside-bindings #'()]
         #:outside-checks [outside-checks #'()]
         #:loop-bindings [loop-bindings #'()]
         #:inside-guards [inside-guards #'()]
         #:inside-bindings [inside-bindings #'()]
         #:inside-checks [inside-checks #'()]
         #:body-guards [body-guards #'()]
         #:body-input [body-input #'(values)]
         #:body-bindings [body-bindings #'()]
         #:body-checks [body-checks #'()]
         #:recurse-guards [recurse-guards #'()]
         #:recurse-arguments [recurse-arguments #'()])
  #`(#,outside-bindings
     #,outside-checks
     #,loop-bindings
     #,inside-guards
     #,inside-bindings
     #,inside-checks
     #,body-guards
     #,body-input
     #,body-bindings
     #,body-checks
     #,recurse-guards
     #,recurse-arguments))
