#lang racket/base

;;;
;;; make-accumulator.rkt
;;;
;;; Provides make-accumulator, a function which can be used to aid the creation of
;;; accumulators.
;;;

(provide (all-defined-out))

(define (make-accumulator
         #:outside-bindings [outside-bindings #'()]
         #:outside-checks [outside-checks #'()]
         #:loop-bindings [loop-bindings #'()]
         #:inside-guards [inside-guards #'()]
         #:return [return #'(void)]
         #:outside-return [outside-return #f]
         #:inside-bindings [inside-bindings #'()]
         #:inside-checks [inside-checks #'()]
         #:body-guards [body-guards #'()]
         #:inside-return [inside-return #f]
         #:body-outputs [body-outputs #'(_)]
         #:body-bindings [body-bindings #'()]
         #:body-checks [body-checks #'()]
         #:recurse-guards [recurse-guards #'()]
         #:body-return [body-return #f]
         #:recurse-arguments [recurse-arguments #'()])
  #`(#,outside-bindings
     #,outside-checks
     #,loop-bindings
     #,inside-guards
     #,(or outside-return return)
     #,inside-bindings
     #,inside-checks
     #,body-guards
     #,(or inside-return outside-return return)
     #,body-outputs
     #,body-bindings
     #,body-checks
     #,recurse-guards
     #,(or body-return inside-return outside-return return)
     #,recurse-arguments))
