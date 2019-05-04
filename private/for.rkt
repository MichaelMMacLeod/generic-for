#lang racket/base

(require (for-syntax racket/base
                     syntax/apply-transformer
                     syntax/parse))

(provide (rename-out [unified-for for]))

(define-syntax (unified-for stx)
  (syntax-parse stx
    [(_ (accumulator accumulator-args ...)
        ([var:id ... (iterator iterator-args ...)] ...)
        body ...)
     (with-syntax
       ([(((pre-bind ...)
           (bind ...)
           (test ...)
           (post-bind ...)
           (step ...))
          ...)
         (map (lambda (i i-args)
                (local-apply-transformer (syntax-local-value i)
                                         i-args
                                         'expression))
              (syntax->list #'(iterator ...))
              (syntax->list #'(((var ...) iterator-args ...) ...)))])
       (with-syntax
         ([((result ...) (a-bind ...) (a-insert ...) (a-collect ...))
           (local-apply-transformer (syntax-local-value #'accumulator)
                                    #'(accumulator-args ...)
                                    'expression)])
         #`(let* (pre-bind ... ...)
             (let loop (bind ... ... a-bind ...)
               (cond [(and test ... ...)
                      (let-values ([(result ...)
                                    (let (post-bind ... ...)
                                      body ...)])
                        (loop step ... ... a-insert ...))]
                     [else (values a-collect ...)])))))]))
