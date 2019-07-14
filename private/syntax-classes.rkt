#lang racket/base

;;;
;;; syntax-classes.rkt
;;;
;;; Provides syntax classes for matching iterators and accumulators.
;;;

(require syntax/parse)

(provide (all-defined-out))

(define-splicing-syntax-class let-values-bindings
  #:attributes ([id 2]
                [expr 1])
  (pattern (~seq [(id:id ...) expr:expr] ...)))

(define-splicing-syntax-class let-bindings
  #:attributes ([id 1]
                [expr 1])
  (pattern (~seq [id:id expr:expr] ...)))

;; When the `unified-for` macro finds accumulators and iterators, it is actually finding
;; identifiers which are (hopefully) bound to syntax transformers. The `accumulator` and
;; `iterator` syntax classes perform the syntax transformer expansion via `local-expand` so we
;; don't have to deal with it in `unified-for`.

(define-syntax-class accumulator
  (pattern unexpanded:expr
           #:with
           ((outside-bindings:let-values-bindings)
            (outside-checks:expr ...)
            (loop-bindings:let-bindings)
            (inside-guards:expr ...)
            outside-return:expr
            (inside-bindings:let-values-bindings)
            (inside-checks:expr ...)
            (body-guards:expr ...)
            inside-return:expr
            (body-outputs:id ...)
            (body-bindings:let-values-bindings)
            (body-checks:expr ...)
            (recurse-guards:expr ...)
            body-return:expr
            (recurse-arguments:expr ...))
           (local-expand (if (identifier? #'unexpanded)
                             (syntax/loc #'unexpanded
                               ; We do this so that (for accumulator-id ....) is equivalent to
                               ; (for (accumulator-id) ....).
                               (unexpanded))
                             #'unexpanded)
                         'expression
                         #f)
           #:with
           ((outside-bindings-id ...) ...)
           #'((outside-bindings.id ...) ...)
           #:with
           (outside-bindings-expr ...)
           #'(outside-bindings.expr ...)
           #:with
           (loop-bindings-id ...)
           #'(loop-bindings.id ...)
           #:with
           (loop-bindings-expr ...)
           #'(loop-bindings.expr ...)
           #:with
           ((inside-bindings-id ...) ...)
           #'((inside-bindings.id ...) ...)
           #:with
           (inside-bindings-expr ...)
           #'(inside-bindings.expr ...)
           #:with
           ((body-bindings-id ...) ...)
           #'((body-bindings.id ...) ...)
           #:with
           (body-bindings-expr ...)
           #'(body-bindings.expr ...)
           ))

(define-syntax-class iterator
  (pattern unexpanded:expr
           #:with
           ((outside-bindings:let-values-bindings)
            (outside-checks:expr ...)
            (loop-bindings:let-bindings)
            (inside-guards:expr ...)
            (inside-bindings:let-values-bindings)
            (inside-checks:expr ...)
            (body-guards:expr ...)
            body-input:expr
            (body-bindings:let-values-bindings)
            (body-checks:expr ...)
            (recurse-guards:expr ...)
            (recurse-arguments:expr ...))
           (local-expand (if (identifier? #'unexpanded)
                             (syntax/loc #'unexpanded
                               ; We do this so that (for .... ([.... iterator-id] ....) ....)
                               ; is equivalent to (for .... ([.... (iterator-id)] ....) ....).
                               (unexpanded))
                             #'unexpanded)
                         'expression
                         #f)
           #:with
           ((outside-bindings-id ...) ...)
           #'((outside-bindings.id ...) ...)
           #:with
           (outside-bindings-expr ...)
           #'(outside-bindings.expr ...)
           #:with
           (loop-bindings-id ...)
           #'(loop-bindings.id ...)
           #:with
           (loop-bindings-expr ...)
           #'(loop-bindings.expr ...)
           #:with
           ((inside-bindings-id ...) ...)
           #'((inside-bindings.id ...) ...)
           #:with
           (inside-bindings-expr ...)
           #'(inside-bindings.expr ...)
           #:with
           ((body-bindings-id ...) ...)
           #'((body-bindings.id ...) ...)
           #:with
           (body-bindings-expr ...)
           #'(body-bindings.expr ...)
           ))
