#lang racket

;; a primitive implementation of json-lang that 

(provide
 (rename-out [mb #%module-begin]
             [ti #%top-interaction])
 #%datum)

;; ---------------------------------------------------------------------------------------------------
(require (for-syntax syntax/parse))
(require (for-syntax racket/format))
(require  syntax/parse/define)

;; ---------------------------------------------------------------------------------------------------
(module reader syntax/module-reader
  json-lang)

;; ---------------------------------------------------------------------------------------------------
(define-syntax (mb stx)
  (syntax-parse stx 
    [(_ e ...)
     (define js #`((json-expression e) ...))
     (define e* (syntax->list js))
     (for-each tc-le e* (syntax->list #`(e ...)))
     #`(#%printing-module-begin #,@js)]))

;; ---------------------------------------------------------------------------------------------------
;; hand-rolled type checker 

(define-for-syntax (tc-le e source)
  (define expanded (local-expand e 'expression [list]))
  (tc expanded source))

#; {Stx Stx -> Type}
;; signal the first type error 
(define-for-syntax (tc stx source)
  (let tc ([stx stx] [env '()])
    (syntax-parse stx
      [x:id (cadr (assq (syntax-e #'x) env))]
      [((~literal let-values) (((x:id) ((~literal begin) ((~literal quote) t:str) rhs))) body)
       (define t-rhs (tc #'rhs env))       
       (cond
         [(equal? t-rhs (syntax-e #'t))
          (tc #'body (cons [list (syntax-e #'x) (syntax-e #'t)] env))]
         [else
          (define msg (~a "type of rhs (" t-rhs ") doesn't match declared type (" (syntax-e #'t) ")"))
           (raise-syntax-error #false msg source #'rhs)])]
      
      [((~literal quote) n:number)  "int"]
      [((~literal quote) b:boolean) "bool"]
      [(_ o a b)
       (define (err [x ""])
         (raise-syntax-error #false (~a "type error" x) source #'a (list #'o #'b)))
       (define left  (tc #'a env))
       (define right (tc #'b env))
       (cond
         [(not (equal? left right)) (err (~a left " vs " right))]
         [else
          (case (syntax-e #'o)
            [(+ -) (if (equal? "int" left) "int" (err "int expected"))]
            [(<)   (if (equal? "int" left) "bool" (err "int expected"))]
            [(andd ord) (if (equal? "bool" left) "bool" (err "bool expected"))]
            [else (err)])])])))

;; ---------------------------------------------------------------------------------------------------
;; expansion to macro 

(begin-for-syntax
  (define-syntax-class operator
    (pattern "+" #:attr op #'+)
    (pattern "-" #:attr op #'-)
    (pattern "||" #:attr op #'ord)
    (pattern "^" #:attr op #'andd)
    (pattern "<" #:attr op #'<))

  (define-syntax-class type
    (pattern "int")
    (pattern "bool")))

(define-syntax (json-expression stx)
  (syntax-parse stx
    [(_ x:integer) #'x]
    [(_ x:boolean) #'x]
    [(_ (left ((~datum unquote) op:operator) ((~datum unquote) right)))
     #'(op.op (json-expression left) (json-expression right))]

    [(_ ("let" ((~datum unquote) t:type) ((~datum unquote) x:str) ((~datum unquote) "=")
               ((~datum unquote) rhs)
               ((~datum unquote) "in")
               ((~datum unquote) body)))
     #`(let ([#,(string->identfier #'x) (begin 't (json-expression rhs))]) (json-expression body))]
    [(_ x:string)
     #`#,(string->identfier #'x)]
    
    [(_ . x) (raise-syntax-error #false "syntax error" #'x #f (syntax->list #'x))]))

(define (andd x y) (and x y))
(define (ord x y) (or x y))

#; {Striing -> Id}
;; convert a string to an identifier with its syntax properties 
(define-for-syntax (string->identfier y)
  (datum->syntax y (string->symbol (syntax-e y)) y y))

(define-syntax (ti stx)
  (syntax-parse stx
    [(_ . form) #'(json-expression form)]))

;; -------

(module+ test
  
  (json-expression
   ["let", "int", "x", "=", (10, "+", 5),
         "in",
         "x"])

  (json-expression
   ["let", "int", "y", "=", 42,
         "in",
         ["let", "int", "x", "=", (10, "+", 5),
               "in",
               "y"]]))

