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
     (define ts (map tc js))
     (define ee (map (λ (t j) [list #'printf "~v : ~a\n" j #`'#,(syntax-e t)]) ts js))
     #`(#%module-begin #,@ee)]))

(define-syntax-rule (annotate t e) e)

;; ---------------------------------------------------------------------------------------------------
;; expansion to macro 

(begin-for-syntax

  ;; doman types for operators 
  (define-syntax-class primitive
    (pattern (~literal +)     #:attr type #'int  #:attr in #'int)
    (pattern (~literal -)     #:attr type #'int  #:attr in #'int)
    (pattern (~literal ord)   #:attr type #'bool #:attr in #'bool)
    (pattern (~literal andd)  #:attr type #'bool #:attr in #'bool)
    (pattern (~literal <)     #:attr type #'bool #:attr in #'int)))

(define-for-syntax (tc e)
  (let tc ([e e]  [env '()])
    (syntax-parse e
      [x:boolean #'bool]
      [x:integer #'int]
      [x:id (lookup #'x env (~a "id: " #'x " " env) e)]
    
      [(op:primitive left right)
       (define t-left  (tc #'left env))
       (define t-right (tc #'right env))
       (check e "operator input types" "computed types: " #'op.in t-left t-right)
       #'op.type]
    
      [(let ([x:id (annotate rt (lambda (y:id) ((~literal annotate) at rhs)))]) body)
       (define t-lambda (tc #'rhs (extend #'y #'at env)))
       (check e  "declared return type" "computed return type" #'rt t-lambda)
       (define t-body (tc #'body (extend #'x #'(-> at rt) env)))
       t-body]
    
      [(let ([x:id (annotate rt rhs)]) body)
       (define t-rhs (tc #'rhs env))
       (check e "declared type:" "computed type:" #'rt t-rhs)
       (define t-body (tc #'body (extend #'x #'rt env)))
       t-body]

      [(begin0 x:id (set! y:id rhs))
       (unless (bound-identifier=? #'x #'y)
         (raise-syntax-error #false "bound identifier =" e #'x (list #'y)))
       (define t-lhs (lookup #'x env (~a "id: " #'x " " env) e))
       (define t-rhs (tc #'rhs env))
       (check e "declared variable type" "computed rhs type:" t-lhs t-rhs)
       t-lhs]

      [(f:id arg)
       (define t-fun (lookup #'f env (~a "application function id:  " env " ") e))
       (define t-arg (tc #'arg env))
       (check e "expected argument type " "actual type: " (->-dom t-fun) t-arg)
       (->-rng t-fun)]
    
      [x (raise-syntax-error #false "type error (no match)" #'x (list #'x))])))

#; {Stx String String TypeStx TypeStx TypeStx *-> Void}
(define-for-syntax (check e pre post expected-type actual-type . more-types)
  (unless (apply type-equal? expected-type actual-type more-types)
    (define a (if (null? more-types) actual-type (cons actual-type more-types)))
    (define msg (format-msg pre expected-type post a))
    (raise-syntax-error #false msg #f #f (syntax-property e 'tsource))))
  
#; {String TypeStx String TypeStx -> String}
(define-for-syntax (format-msg pre s post t)
  (define uu (if (pair? t) (map syntax-e t) (syntax-e t)))
  (~a pre ": " (syntax-e s) " vs " post ": " uu))

#; {TypeStx -> TypeStx}
(define-for-syntax (->-dom t)
  (syntax-parse t
    [((~datum ->) dom rng) #'dom]
    [_ #false]))

#; {TypeStx -> TypeStx}
(define-for-syntax (->-rng t)
  (syntax-parse t
    [((~datum ->) dom rng) #'rng]
    [_ #false]))

#; {SymbolStx TEnv String Stx -> TypeStx}
(define-for-syntax (lookup x env msg e)
  (define tid (assq (syntax-e x) env))
  (unless tid
    (raise-syntax-error #false msg e (list x)))
  (cadr tid))

#; {SymbolStx TypeStx TEnv -> TEnv}
(define-for-syntax (extend x v env)
  (define y (syntax-e x))
  (cons [list y v] env))

#; {TypeStx TypeStx TypeStx *-> Boolean}
(define-for-syntax (type-equal? s t . u)
  (cond
    [(null? u) (equal? (syntax-e s) (syntax-e t))]
    [else      (and (equal? (syntax-e s) (syntax-e t)) (apply type-equal? t u))]))

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
     (define op #`(op.op #,(json-expression #'left) #,(json-expression #'right)))
     (syntax-property op 'tsource [list #'left #'op #'right])]
    
    ;; plain let for declaring variables 
    [("let" ((~datum unquote) t:type) ((~datum unquote) x:str) ((~datum unquote) "=")
            ((~datum unquote) rhs)
            ((~datum unquote) "in")
            ((~datum unquote) body))
     (json-let #'t.type #'x #'rhs #'body)]
    
    ;; let for a language that also has first-order functions 
    [("let" ((~datum unquote) "var")
            ((~datum unquote) t:type) ((~datum unquote) x:str) ((~datum unquote) "=")
            ((~datum unquote) rhs)
            ((~datum unquote) "in")
            ((~datum unquote) body))
     (json-let #'t.type #'x #'rhs #'body)]

    ;; assignment statement 
    [(x:str ((~datum unquote) "=") ((~datum unquote) rhs))
     (define y (string->identfier #'x))
     (define a #`(begin0 #,y (set! #,y #,(json-expression #'rhs))))
     (syntax-property a 'tsource (list #'x #'rhs))]
    
    ;; varibales 
    [x:string #`#,(string->identfier #'x)]

    ;; let for declaring functions 
    [("let" ((~datum unquote) "fun")
            ((~datum unquote) rt:type) ((~datum unquote) f:str)
            ((~datum unquote) [at:type ((~datum unquote) x:str)])
            ((~datum unquote) rhs)
            ((~datum unquote) "in")
            ((~datum unquote) body))
     (json-fun #'rt.type #'f #'at.type #'x #'rhs #'body)]
    
    [("call" ((~datum unquote) f:str) ((~datum unquote) a))
     (define a-cal #`(#,(string->identfier #'f) #,(json-expression #'a)))
     (syntax-property a-cal 'tsource (list #'f #'a))]
   
    [x (raise-syntax-error #false "syntax error (no match)" #'x #f (list #'x))]))

#; {TypeStx StrStx Stx Stx -> Stx + tsource property}
(define-for-syntax (json-let t x rhs body)
  (define a-rhs #`(annotate #,t #,(json-expression rhs)))
  (define a-let #`(let ([#,(string->identfier x) #,a-rhs]) #,(json-expression body)))
  (syntax-property a-let 'tsource (list t x rhs)))

#; {TypeStx StrStx TypeStx StrStx Stx Stx -> Stx + tsource property}
(define-for-syntax (json-fun rt f at x rhs body)
  (define y (string->identfier x))
  (define l #`(annotate #,rt (lambda (#,y) (annotate #,at #,(json-expression rhs)))))
  (define a-let #`(let ([#,(string->identfier f) #,l]) #,(json-expression body)))
  (syntax-property a-let 'tsource (list f x rhs)))

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