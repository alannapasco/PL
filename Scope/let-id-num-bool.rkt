#! /bin/sh
#| -*- racket -*-
exec racket -tm "$0" -- ${1+"$@"}
|#
#lang typed/racket

(provide main)

;; -----------------------------------------------------------------------------
(require typed/json)
(module+ test
  (require "../../Git/Lib/test-framework.rkt")
  (require typed/rackunit))

;; -----------------------------------------------------------------------------
;; interpret a JSON representation of simple mixed expressions 

(define (main)
  (define input  : (U EOF JSExpr)  (read-json))
  (cond [(eof-object? input) (void)] [else (write-json (pipe input)) (newline)]))

(define-type Meaning (U Boolean Integer))

(: pipe (JSExpr -> Meaning))
(define (pipe input)
  (define ast (parse input))
  (define ___ (tc ast '[]))
  (define out (value ast '[]))
  out)

;; -----------------------------------------------------------------------------
;; parsing JSExpr to make sure they are Boolean JSExpr

(struct ERR ([x : Any]))

(define-type Type (U 'int 'bool))

(define-type E-AST (U Boolean
                      [List '&& E-AST E-AST]
                      [List '|| E-AST E-AST]

                      Integer
                      [List '++ E-AST E-AST]
                      [List '-- E-AST E-AST]
                      [List '<< E-AST E-AST]

                      [List 'let (U ERR Type) Symbol E-AST E-AST]
                      Symbol 
                      
                      ERR))

(define-type AST (U Boolean [List '&& AST AST] [List '|| AST AST]
                    Integer [List '++ AST AST] [List '-- AST AST]
                    [List 'let (U ERR Type) Symbol AST AST]
                    Symbol
                    [List '<< AST AST] ))

(: parse (JSExpr -> AST))
(define (parse j)
  (define q (e-parse j))
  (with-handlers ([exn:fail? (λ _ (error 'parse "JSExpr expected, given ~e" q))])
    (cast q AST)))

(: e-parse [JSExpr -> E-AST])
(define (e-parse j)
  (match j
    [(? boolean?) j]
    [(list j-left "||" j-right) (list '|| (e-parse j-left) (e-parse j-right))]
    [(list j-left "^" j-right)  (list '&& (e-parse j-left) (e-parse j-right))]
    [(list j-left "+" j-right)  (list '++ (e-parse j-left) (e-parse j-right))]
    [(? exact-integer?) j]
    [(list j-left "+" j-right)  (list '++ (e-parse j-left) (e-parse j-right))]
    [(list j-left "-" j-right)  (list '-- (e-parse j-left) (e-parse j-right))]
    [(list j-left "<" j-right)  (list '<< (e-parse j-left) (e-parse j-right))]
    
    [(list "let" j-type (? string? x) "=" j-rhs "in"j-body)
     (list 'let (t-parse j-type) (string->symbol x) (e-parse j-rhs) (e-parse j-body))]
    [(? string? id) (string->symbol id)]
    
    [_ (ERR j)]))

(: t-parse (JSExpr -> (U ERR Type)))
(define (t-parse j)
  (match j
    ["int" 'int]
    ["bool" 'bool]
    [_ (ERR j)]))

;; -----------------------------------------------------------------------------
;; type check AST 

(define-type TEnv [Listof [List Symbol Type]])

(: tc (AST TEnv -> Type))
(define (tc b env)
  (match b
    [(? boolean? b) 'bool]
    [`(,(or '|| '&&) ,left  ,right)
     (match* ((tc left env) (tc right env))
       [('bool 'bool) 'bool]
       [(_ _) (error 'tc "&& or ||")])]
    [(? exact-integer? b) 'int]
    [`(,(or '++ '--) ,left  ,right)
     (match* ((tc left env) (tc right env))
       [('int 'int) 'int]
       [(_ _) (error 'tc "++ or --")])]
    [`(<< ,left  ,right)
     (match* ((tc left env) (tc right env))
       [('int 'int) 'bool]
       [(_ _) (error 'tc "<<")])]

    [`(let ,t ,x ,rhs ,body)
     (define t-rhs (tc rhs env))
     (unless (equal? t t-rhs)
       (error 'tc "types don't match: ~a ~a \n ~a\n" t t-rhs b))
     (tc body (cons [list x t] env))]
    [(? symbol? x)
     (define t (assq x env))
     (unless t
       (error 'tc "identifier undefined: ~a\n" b))
     (second t)]))
                      
;; -----------------------------------------------------------------------------
;; determine the standard meaning of b

(define-type Env (Listof [List Symbol Meaning]))

(define-syntax-rule (bcast e) (cast e Boolean))
(define-syntax-rule (icast e) (cast e Integer))

(: value (-> AST Env Meaning))
(define (value b env)
  (match b
    [(? exact-integer? b) b]
    [(? boolean? b) b]
    [`(&& ,left  ,right) (and (bcast (value left env)) (bcast (value right env)))]
    [`(|| ,left ,right)  (or  (bcast (value left env)) (bcast (value right env)))]
    [(? exact-integer? b) b]
    [`(++ ,left  ,right) (+ (icast (value left env)) (icast (value right env)))]
    [`(-- ,left  ,right) (- (icast (value left env)) (icast (value right env)))]
    [`(<< ,left  ,right) (< (icast (value left env)) (icast (value right env)))]
    [`(let ,t ,x ,rhs ,body) (value body (cons [list x (value rhs env)] env))]
    [(? symbol? x)
     (define v (assq x env))
     (unless v
       (error 'value "can't happen ~a" x))
     (second v)]))

;; -----------------------------------------------------------------------------
;; -----------------------------------------------------------------------------
;; integration and unit tests

(module+ test ;; integration tests for `main`
  (test main '(#true "^" #false)  #false "and false")
  (test main '(#true "||" #false) #true  "or true")
  (test main '(#true "|" #false)  #false "exn" exn:fail?)
  (test main '(5 "+" 0)  5 "numbers")
  (test main '(((5 "+" 0) "<" 10) "^" (#false "||" #true))  #true "complex")
  (test main '["let" "int" "x" "=" (10 "+" 5)
                     "in"
                     "x"]
        15 "simple let")

  (test main '["let" "int" "y" "=" 42
                     "in"
                     ["let" "int" "x" "=" (10 "+" 5)
                            "in"
                            "y"]]
        42 "nested let")

  (test main '["let" "int" "y" "=" 42
                     "in"
                     ["let" "int" "x" "=" (10 "+" 5)
                            "in"
                            ["y" "+" "x"]]]
        57 "nested let"))

(module+ test ;; unit tests for `parse`
  (check-equal? (parse '(#true "^" #false))  '[&& #t #f] "parse and false")
  (check-equal? (parse '(#true "||" #false)) '[|| #t #f] "parse or true")
  (check-exn exn:fail? (λ () (parse '(#true "|" #false))) "exn"))
