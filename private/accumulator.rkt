#lang racket/base

(require (for-syntax racket/base
                     syntax/parse))

(provide to-fold
         to-hash-set
         to-list)

(define-syntax to-hash-set
  (syntax-parser
    [()
     #'((last-body)
        ([acc (hash)])
        ((hash-set acc last-body #t))
        acc)]))

(define-syntax to-list
  (syntax-parser
    [()
     (with-syntax ([(last-body) (generate-temporaries #'(tmp))])
       #'((last-body)
          ([acc null])
          ((cons last-body acc))
          (reverse acc)))]))

(define-syntax to-fold
  (syntax-parser
    #:track-literals
    [([fold-var:id start:expr]
      ...
      (~optional (~seq #:result result:expr)
                 #:defaults ([result #'(values fold-var ...)])))
     (with-syntax ([(last-body ...) (generate-temporaries #'((fold-var start) ...))])
       #'((last-body ...)
          ([fold-var start] ...)
          (last-body ...)
          result))]))
