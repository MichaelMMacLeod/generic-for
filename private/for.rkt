#lang racket/base

(require racket/match
         (for-syntax racket/base
                     syntax/parse))

(require "accumulator.rkt"
         "iterator.rkt")

(provide (rename-out [generic-for for]))

(define-syntax (generic-for stx)
  (syntax-parse stx
    [(_ accumulator
        ([(pattern ...) iterator] ...)
        body ...)
     (with-syntax ([(tmps ...)
                    (generate-temporaries #'(iterator ...))])
       #'(let loop ([acc (Accumulator-empty accumulator)]
                    [tmps (Iterator-collection iterator)] ...)
           (cond [(and ((Iterator-not-empty? iterator) tmps) ...)
                  (match-define-values (pattern ...)
                                       ((Iterator-take iterator) tmps)) ...
                  (loop (call-with-values (lambda () body ...)
                                          (lambda x
                                            (apply (Accumulator-insert accumulator)
                                                   acc x)))
                        ((Iterator-drop iterator) tmps) ...)]
                 [else ((Accumulator-collect accumulator) acc)])))]))
