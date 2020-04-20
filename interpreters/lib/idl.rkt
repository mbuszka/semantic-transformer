#lang racket

(require
  (for-syntax syntax/parse
              racket/syntax))

(provide def-data
         def-struct
         def
         integer
         string
         boolean)

(begin-for-syntax
  (define-syntax-class type
    #:attributes (tp contract struct-def)
    (pattern tp:type-literal
             #:with contract (format-id #'tp "~a?" #'tp)
             #:attr struct-def #f)
    (pattern tp:id
             #:attr struct-def #f
             #:with contract (format-id #'tp "~a/c" #'tp))
    (pattern (tp:id field:type ...)
             #:with c-name (format-id #'name "~a/c" #'tp)
             #:with contract #'(recursive-contract c-name #:flat)
             #:with (fields ...) (generate-temporaries #'(field.tp ...))
             #:with struct-def
             #'((struct tp (fields ...) #:prefab)
                (define c-name (struct/c tp field.contract ...)))
             ))

  (define-syntax-class type-literal
    (pattern (~literal integer))
    (pattern (~literal string))
    (pattern (~literal boolean)))

  (define-syntax-class def-arg
    (pattern name:id
             #:with contract #'any/c)
    (pattern [name:id tp:type]
             #:with contract #'tp.contract))
  )

(define-syntax (def-data stx)
    (syntax-parse stx
      [(_ name:id elems:type ...)
       #:with ((struct-def contract-def) ...) #'((~? elems.struct-def) ...)
       #:with c-name (format-id #'name "~a/c" #'name)
       #'(begin
           (define c-name (flat-named-contract 'name (or/c elems.contract ...)))
           struct-def ...
           contract-def ...
           )]))

(define-syntax (def-struct stx)
  (syntax-parse stx
    [(_ name:id cnt:integer)
     #:with (elems ...) (generate-temporaries (build-list (syntax->datum #'cnt) (lambda (_) #'name)))
     #'(struct name (elems ...))]))

(define-syntax (def stx)
  (syntax-parse stx
    [(_ name:id (arg:def-arg ...) body)
     #:with contract #'(-> arg.contract ... any/c)
     #'(begin
         (define/contract (name arg.name ...) contract body))]))

(define-syntax (define-base-types stx)
  (syntax-parse stx
    [(_ type:id ...)
     #:with (pred ...) (for/list ([tp (syntax->list #'(type ...))]) (format-id tp "~a?" tp))
     #'(begin
         (define-match-expander type (lambda (stx) (syntax-case stx () [(_ p) #'(? pred p)]))) ...)]))

(define-base-types integer string boolean)