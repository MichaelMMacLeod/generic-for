#lang scribble/manual

@(require racket/contract/base
          (for-label unified-for
                     racket/contract/base
                     (except-in racket/base for)))

@title{Unified for Loops}

@author{Michael MacLeod}

@defmodule[unified-for]

This package unifies the various flavors of @racket[for] iteration
constructs---@racket[for], @racket[for/list], @racket[for/vector]---into a single
@racket[for] macro.

@defform/subs[(for maybe-accumulator (for-clause ...) body ...)
              [(maybe-accumulator (code:line)
                                  accumulator-expr
                                  (accumulator-expr arg-expr ...))
               (for-clause [pattern ... iterator-expr])]
              #:contracts ([accumulator-expr Accumulator?]
                           [iterator-expr Iterator?])]{
}

@defstruct*[Accumulator ([empty collection?]
                         [insert (-> collection? element? collection?)]
                         [collect (-> collection? collection?)])]{
}

@defstruct*[Iterator ([take (-> collection? element?)]
                      [drop (-> collection? collection?)]
                      [not-empty? (-> collection? boolean?)]
                      [collection collection?])]{
}

@deftogether[(@defthing[collection? any/c]
               @defthing[element? any/c])]{
}