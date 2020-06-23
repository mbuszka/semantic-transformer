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

  (define-splicing-syntax-class kw-arg
    (pattern (~seq _:keyword _:id))
    (pattern _:keyword)
    )

  (define-syntax-class typed-arg
    (pattern name:id
             #:with contract #'any/c)
    (pattern [tp:type name:id]
             #:with contract #'tp.contract))

  (define-syntax-class pat
    (pattern (~literal _))
    (pattern _:id)
    (pattern (tp:type pat:pat ...))
    (pattern _:string)
    (pattern _:boolean)
    (pattern _:integer))

  (define-syntax-class statement
    (pattern ((~literal let) ~! pat:pat t:term)
             #:with term #'t.res))

  (define-syntax-class statements
    (pattern (~and (~var all) (lets:statement ... term:term))
             #:with res (syntax/loc #'all (match-let* ([lets.pat lets.term] ...) term.res))))

  (define-syntax-class term
    (pattern (~and (~var all) ((~literal match) ~! t:term bs:branch ...))
             #:with res (syntax/loc #'all (match t.res bs.res ...)))
    (pattern (~and (~var all) ((~literal fun) ~! _:kw-arg ... (arg:id ...) . body:statements))
             #:with res (syntax/loc #'all (lambda (arg ...) body.res)))
    (pattern (~and (~var all) (f:term xs:term ...))
             #:with res (syntax/loc #'all (f.res xs.res ...)))
    (pattern res:string)
    (pattern res:boolean)
    (pattern res:integer)
    (pattern res:id))

  (define-syntax-class branch
    (pattern (pat:pat . stmts:statements)
             #:with res #'(pat stmts.res)))

  (define (tp->var stx)
    (datum->syntax stx (string->symbol (string-downcase (symbol->string (syntax->datum stx))))))
  )

(define-syntax (def-data stx)
  (syntax-parse stx
    [(_ name:id elems:data-elem ...)
     #:with ((struct-def contract-def) ...) #'((~? elems.struct-def) ...)
     #:with c-name (format-id #'name "~a/c" #'name)
     #'(begin
         ; (define c-name (flat-named-contract 'name (or/c elems.contract ...)))
         struct-def ...
         ; contract-def ...
       )]))

(define-syntax (def-struct stx)
  (syntax-parse stx
    [(_ struct:struct-def)
     #:with (s c) #'struct.struct-def
     ; #'(begin s c)]))
     #'s]))

(define-syntax (def stx)
  (syntax-parse stx
    [(_ name:id _:kw-arg ... (arg:typed-arg ...) . body:statements)
     #:with contract #'(-> arg.contract ... any/c)
     ; #'(begin
     ;    (define/contract (name arg.name ...) contract body.res))]))
     #'(define (name arg.name ...) body.res)]))

;(define-syntax (fun stx)
;  (syntax-parse stx
;    [(_ _:keyword ... (arg:id ...) . body:statements)
;     #'(lambda (arg ...) body.res)]))

(define-match-expander Integer
  (lambda (stx) (syntax-case stx () [(_ p) #'(? integer? p)])))

(define-match-expander String
  (lambda (stx) (syntax-case stx () [(_ p) #'(? string? p)])))

(define-match-expander Boolean
  (lambda (stx) (syntax-case stx () [(_ p) #'(? boolean? p)])))