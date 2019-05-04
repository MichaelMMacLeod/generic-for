#lang racket/base

(require racket/match
         (for-syntax racket/base
                     syntax/parse))

(require "accumulator.rkt"
         "iterator.rkt"
         "void.rkt")

(provide (rename-out [generic-for for]))

(define-syntax (generic-for stx)
  (syntax-parse stx
    [(_ (~optional (~or accumulator:id accumulator:expr)
                   #:defaults ([accumulator #'to-void]))
        ([pattern ... iterator] ...)
        body ...)
     (with-syntax ([(collection ...)
                    (generate-temporaries #'(iterator ...))]
                   [((taken-values ...) ...)
                    (map generate-temporaries (syntax->list #'((pattern ...) ...)))]
                   [(no-match-id ...)
                    (generate-temporaries (syntax->list #'((pattern ...) ...)))]
                   [accumulator
                    (if (identifier? #'accumulator)
                        #'(accumulator)
                        #'accumulator)])
       #'(let loop ([acc (Accumulator-empty accumulator)]
                    [collection (Iterator-collection iterator)] ...)
           (cond [(and ((Iterator-not-empty? iterator) collection) ...)
                  (define-values (taken-values ...)
                    ((Iterator-take iterator) collection))
                  ...
                  (match* (taken-values ... ...)
                    [(pattern ... ...)
                     (loop (call-with-values (lambda () body ...)
                                             (lambda x
                                               (apply (Accumulator-insert accumulator)
                                                      acc x)))
                           ((Iterator-drop iterator) collection) ...)]
                    [(no-match-id ...) ((Accumulator-collect accumulator) acc)])]
                 [else ((Accumulator-collect accumulator) acc)])))]))
