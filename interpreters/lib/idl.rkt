#lang racket

(require
  (for-syntax syntax/parse
              racket/syntax))

(provide def-data
         def-struct
         def
         Integer
         String
         Boolean)

(begin-for-syntax
  (define-syntax-class type-literal
    (pattern (~literal Integer)
             #:with contract #'integer?)
    (pattern (~literal String)
             #:with contract #'string?)
    (pattern (~literal Boolean)
             #:with contract #'boolean?)
    (pattern (~literal Any)
             #:with contract #'any/c))

  (define-syntax-class type
    #:attributes (name contract)
    (pattern name:type-literal
             #:with contract #'name.contract)
    (pattern name:id
             #:when (char-upper-case? (string-ref (symbol->string (syntax->datum #'name)) 0))
             #:with contract (format-id #'name "~a/c" #'name)))

  (define-syntax-class struct-field
    #:attributes (name contract)
    (pattern tp:type
             #:with contract #'tp.contract
             #:with (name) (generate-temporaries (list (tp->var #'tp))))
    (pattern name:id
             #:with contract #'any/c)
    (pattern (tp:type name:id)
             #:with contract #'tp.contract))

  (define-syntax-class struct-def
    (pattern (tp:type field:struct-field ...)
             #:with c-name (format-id #'tp "~a/c" #'tp)
             #:with contract #'(recursive-contract c-name #:flat)
             #:with struct-def
             #'((struct tp (field.name ...) #:prefab)
                (define c-name (struct/c tp field.contract ...)))))
  (define-syntax-class data-elem
    #:attributes (contract struct-def)
    (pattern tp:type
             #:with contract #'tp.contract
             #:attr struct-def #f)
    (pattern str:struct-def
             #:with contract #'str.contract
             #:with struct-def #'str.struct-def))

  (define-syntax-class def-arg
    (pattern name:id
             #:with contract #'any/c)
    (pattern [tp:type name:id]
             #:with contract #'tp.contract))

  (define (tp->var stx)
    (datum->syntax stx (string->symbol (string-downcase (symbol->string (syntax->datum stx))))))
  
  )

(define-syntax (def-data stx)
  (syntax-parse stx
    [(_ name:id elems:data-elem ...)
     #:with ((struct-def contract-def) ...) #'((~? elems.struct-def) ...)
     #:with c-name (format-id #'name "~a/c" #'name)
     #'(begin
         (define c-name (flat-named-contract 'name (or/c elems.contract ...)))
         struct-def ...
         contract-def ...)]))

(define-syntax (def-struct stx)
  (syntax-parse stx
    [(_ struct:struct-def)
     #:with (s c) #'struct.struct-def
     #'(begin s c)]))

(define-syntax (def stx)
  (syntax-parse stx
    [(_ name:id _:keyword ... (arg:def-arg ...) body)
     #:with contract #'(-> arg.contract ... any/c)
     #'(begin
         (define/contract (name arg.name ...) contract body))]))

(define-match-expander Integer
  (lambda (stx) (syntax-case stx () [(_ p) #'(? integer? p)])))

(define-match-expander String
  (lambda (stx) (syntax-case stx () [(_ p) #'(? string? p)])))

(define-match-expander Boolean
  (lambda (stx) (syntax-case stx () [(_ p) #'(? boolean? p)])))