#lang racket

;; a primitive implementation of json-lang that 

(provide
 (rename-out [mb #%module-begin]
             [ti #%top-interaction])
 #%datum)

;; ---------------------------------------------------------------------------------------------------
(require (for-syntax syntax/parse))
(require (for-syntax racket/format))
(require (for-syntax racket/match))
(require syntax/parse/define)

;; ---------------------------------------------------------------------------------------------------
(module reader syntax/module-reader
  json-lang)

;; ---------------------------------------------------------------------------------------------------
(define-syntax (mb stx)
  (syntax-parse stx 
    [(_ e ...)
     (define e* (syntax->list #'(e ...)))
     (define js (map json-expression e*))
     (for-each tc js)
     #`(#%printing-module-begin #,@js)]))

(define-syntax-rule (annotate t e) e)

;; ---------------------------------------------------------------------------------------------------
;; expansion to macro 

(begin-for-syntax

  ;; doman types for operators 
  (define-syntax-class primitive
    (pattern (~literal +)     #:attr type #'int)
    (pattern (~literal -)     #:attr type #'int)
    (pattern (~literal ord)   #:attr type #'bool)
    (pattern (~literal andd)  #:attr type #'bool)
    (pattern (~literal <)     #:attr type #'bool)))

(define-for-syntax (tc e [env '()])
  (syntax-parse e
    [x:boolean #'bool]
    [x:integer #'int]
    [x:id (lookup #'x env (~a "id: " #'x " " env) e)]
    
    [(op:primitive left right)
     (define t-left  (tc #'left env))
     (define t-right (tc #'right env))
     (unless (type-equal? t-left t-right)
       (raise-syntax-error #false "operator" e (list #'op)))
     #'op.type]
    
    [(let ([x:id (annotate rt (lambda (y:id) ((~literal annotate) at rhs)))]) body)
     (define t-lambda (tc #'rhs (extend #'y #'at env)))
     (unless (type-equal? t-lambda #'rt)
       (define msg (~a "let-λ: " #'rt " " t-lambda))
       (raise-syntax-error #false msg e (list #'rhs)))
     (define t-body (tc #'body (extend #'x #'(-> at rt) env)))
     t-body]
    
    [(let ([x:id (annotate rt rhs)]) body)
     (define t-lambda (tc #'rhs env))
     (unless (type-equal? t-lambda #'rt)
       (define msg (~a "declared type: " (syntax-e #'rt) " vs computed type: " (syntax-e t-lambda)))
       (match-define [list st sx srhs sbody] (syntax-property e 'tsource))
       (raise-syntax-error #false msg #false #false [list st sx srhs]))
     (define t-body (tc #'body (extend #'x #'rt env)))
     t-body]

    [(begin0 x:id (set! y:id rhs))
     (unless (bound-identifier=? #'x #'y)
       (raise-syntax-error #false "bound identifier =" e #'x (list #'y)))

     (define t (lookup #'x env (~a "id: " #'x " " env) e))
     (define t-rhs (tc #'rhs env))
     (unless (type-equal? t-rhs t)
       (define msg (~a "expected type: " (syntax-e t) " vs computed type: " (syntax-e t-rhs)))
       (match-define [list sx srhs] (syntax-property e 'tsource))
       (raise-syntax-error #false msg #false #false [list sx srhs]))
     t]
     
    [(f:id arg)
     (define t-fun (lookup #'f env (~v "application function id:  " env " ") e))
     (define t-arg (tc #'arg env))
     (unless (type-equal? (->-dom t-fun) t-arg)
       (define msg (~a "call " t-arg " vs " t-fun))
       (raise-syntax-error #false msg e e))
     (->-rng t-fun)]
    
    [x (raise-syntax-error #false "type error (no match)" #'x (list #'x))]))

(define-for-syntax (->-dom t)
  (syntax-parse t
    [((~datum ->) dom rng) #'dom]
    [_ #false]))

(define-for-syntax (->-rng t)
  (syntax-parse t
    [((~datum ->) dom rng) #'rng]
    [_ #false]))

(define-for-syntax (lookup x env msg e)
  (define tid (assq (syntax-e x) env))
  (unless tid
    (raise-syntax-error #false msg e (list x)))
  (cadr tid))

(define-for-syntax (extend x v env)
  (define y (syntax-e x))
  (cons [list y v] env))

(define-for-syntax (type-equal? s t)
  (equal? (syntax-e s) (syntax-e t)))

;; ---------------------------------------------------------------------------------------------------
;; JSON to Racket conversion 

(begin-for-syntax

  ;; JSON operators 
  (define-syntax-class operator
    (pattern "+"  #:attr op #'+    )
    (pattern "-"  #:attr op #'-    )
    (pattern "||" #:attr op #'ord  )
    (pattern "^"  #:attr op #'andd )
    (pattern "<"  #:attr op #'<    ))

  ;; JSON types 
  (define-syntax-class type
    (pattern "int"  #:attr type #'int)
    (pattern "bool" #:attr type #'bool)
    ;; this last one will be needed for higher-order functions 
    #;
    (pattern ((~datum '->) a:type b:type) #:attr type #`(-> #,#'a.type #,#'b.type))))

(define-for-syntax (json-expression stx)
  (syntax-parse stx
    [x:integer #'x]
    [x:boolean #'x]
    [(left ((~datum unquote) op:operator) ((~datum unquote) right))
     #`(op.op #,(json-expression #'left) #,(json-expression #'right))]
    
    ;; plain let for declaring variables 
    [("let" ((~datum unquote) t:type) ((~datum unquote) x:str) ((~datum unquote) "=")
            ((~datum unquote) rhs)
            ((~datum unquote) "in")
            ((~datum unquote) body))
     (syntax-property
      #`(let ([#,(string->identfier #'x) (annotate t.type #,(json-expression #'rhs))])
          #,(json-expression #'body))
      'tsource (list #'t #'x #'rhs #'body))]
    ;; let for a language that also has first-order functions 
    [("let" ((~datum unquote) "var")
            ((~datum unquote) t:type) ((~datum unquote) x:str) ((~datum unquote) "=")
            ((~datum unquote) rhs)
            ((~datum unquote) "in")
            ((~datum unquote) body))
     #`(let ([#,(string->identfier #'x) (annotate t.type #,(json-expression #'rhs))])
         #,(json-expression #'body))]
    [(x:str ((~datum unquote) "=") ((~datum unquote) rhs))
     (define y (string->identfier #'x))
     (syntax-property
      #`(begin0 #,y (set! #,y #,(json-expression #'rhs)))
      'tsource (list #'x #'rhs))]

    ;; varibales 
    [x:string #`#,(string->identfier #'x)]

    ;; let for declaring functions 
    [("let" ((~datum unquote) "fun")
            ((~datum unquote) rt:type) ((~datum unquote) f:str)
            ((~datum unquote) [at:type ((~datum unquote) x:str)])
            ((~datum unquote) rhs)
            ((~datum unquote) "in")
            ((~datum unquote) body))
     (define y (string->identfier #'x))
     (define l #`(annotate rt.type (lambda (#,y) (annotate at.type #,(json-expression #'rhs)))))
     #`(let ([#,(string->identfier #'f) #,l])
         #,(json-expression #'body))]
    [("call" ((~datum unquote) f:str) ((~datum unquote) a))
     #`(#,(string->identfier #'f) #,(json-expression #'a))]
   
    [x (raise-syntax-error #false "syntax error (no match)" #'x #f (list #'x))]))

(define (andd x y) (and x y))
(define (ord x y) (or x y))

#; {Striing -> Id}
;; convert a string to an identifier with its syntax properties 
(define-for-syntax (string->identfier y)
  (datum->syntax y (string->symbol (syntax-e y)) y y))

(define-syntax (ti stx)
  (syntax-parse stx
    [(_ . form) (json-expression #'form)]))

;; -------

(begin-for-syntax
  (module+ test
    (printf "~a\n" (tc (json-expression #'10)))

    (printf "~a\n" (tc (json-expression #'(10, "+", 5))))

    (printf "~a\n" 
            (tc (json-expression
                 #'["let", "var", "int", "x", "=", (10, "+", 5),
                         "in",
                         "x"])))

    (printf "~a\n" 
            (tc
             (json-expression
              #'["let", "int", "y", "=", 42,
                      "in",
                      ["let", "int", "x", "=", (10, "+", 5),
                            "in",
                            "y"]])))))