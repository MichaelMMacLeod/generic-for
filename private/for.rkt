#lang racket/base

(require racket/match
         (for-syntax racket/base
                     syntax/parse))

(require "from-transformer.rkt"
         "to-transformer.rkt")

(provide (rename-out [generic-for for]))

(define-syntax (generic-for stx)
  (syntax-parse stx
    [(_ to-transformer
        ([(pattern ...) from-transformer] ...)
        body ...)
     (with-syntax ([(tmps ...)
                    (generate-temporaries #'(from-transformer ...))])
       #'(let loop ([acc (To-Transformer-empty to-transformer)]
                    [tmps (From-Transformer-collection from-transformer)] ...)
           (cond [(and ((From-Transformer-not-empty? from-transformer) tmps) ...)
                  (match-define-values (pattern ...)
                                       ((From-Transformer-take from-transformer) tmps)) ...
                  (loop (call-with-values (lambda () body ...)
                                          (lambda x
                                            (apply (To-Transformer-insert to-transformer)
                                                   acc x)))
                        ((From-Transformer-drop from-transformer) tmps) ...)]
                 [else ((To-Transformer-collect to-transformer) acc)])))]))
