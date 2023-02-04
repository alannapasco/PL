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

(define-type Meaning (U Boolean Integer Closure))
(define-type Closure [List Symbol AST Env])
(define-type Env (Listof [List Symbol Meaning]))

(: pipe (JSExpr -> JSExpr))
(define (pipe input)
  (define ast (parse input))
  (define ___ (tc ast '[]))
  (define out (value ast '[]))
  (meaning->jsexpr out))

(: meaning->jsexpr (-> Meaning JSExpr))
(define (meaning->jsexpr m)
  (match m
    [(? boolean?) m]
    [(? integer?) m]
    [_ "closure"]))

;; -----------------------------------------------------------------------------
;; parsing JSExpr to make sure they are Boolean JSExpr

(struct ERR ([x : Any]))

(define-type Type (U 'int 'bool (List '-> Type Type)))

(define-type E-AST (U Boolean
                      [List '&& E-AST E-AST]
                      [List '|| E-AST E-AST]

                      Integer
                      [List '++ E-AST E-AST]
                      [List '-- E-AST E-AST]
                      [List '<< E-AST E-AST]

                      [List 'let (U ERR Type) Symbol E-AST E-AST]
                      Symbol

                      [List 'fun (U ERR Type) Symbol Symbol E-AST E-AST]
                      [List 'cal Symbol E-AST]
                      
                      ERR))

(define-type AST (U Boolean [List '&& AST AST] [List '|| AST AST] [List '<< AST AST]
                    Integer [List '++ AST AST] [List '-- AST AST]
                    [List 'let (U ERR Type) Symbol AST AST]
                    Symbol
                    [List 'fun Type Symbol Symbol AST AST]
                    [List 'cal Symbol AST]))

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
    
    [(list "let" "var" j-type (? string? x) "=" j-rhs "in" j-body)
     (list 'let (t-parse j-type) (string->symbol x) (e-parse j-rhs) (e-parse j-body))]
    [(? string? id) (string->symbol id)]

    [(list "let" "fun" rj-type (? string? f) [list aj-type (? string? x)] j-rhs "in" j-body)
     (define a-type (t-parse aj-type))
     (define r-type (t-parse rj-type))
     (define ->type (fun-type a-type r-type))
     (define fname  (string->symbol f))
     (define aname  (string->symbol x))
     (list 'fun ->type fname aname (e-parse j-rhs) (e-parse j-body))]
    [(list "call" (? string? f) j-arg)
     (list 'cal (string->symbol f) (e-parse j-arg))]
    
    [_ (ERR j)]))

(: fun-type ((U ERR Type) (U ERR Type) -> (U ERR (List '-> Type Type))))
(define (fun-type a-type r-type)
  (cond
    [(ERR? a-type) (ERR "bad arg type")]
    [(ERR? r-type) (ERR "bad return type")]
    [else (list '-> a-type r-type)]))

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
     (second t)]

    [`(fun (-> ,p-type ,r-type) ,f ,a ,rhs ,body)
     (define t-rhs (tc rhs (cons [list a p-type] env)))
     (unless (equal? t-rhs r-type)
       (error 'tc "return types don't match: ~a \n ~a\n" t-rhs p-type))
     (tc body (cons [list f `(-> ,p-type ,r-type)] env))]
    [`(cal ,f ,a)
     (define t (assq f env))
     (unless t
       (error 'tc "function undefined: ~a\n~a\n" b env))
     (match (second t)
       [`(-> ,p-type ,r-type)
        (define a-type (tc a env))
        (unless (equal? p-type a-type)
          (error 'tc "argument types don't match: ~a \n ~a\n" p-type a-type))
        r-type]
       [_ (error 'tc "function undefined: ~a\n" f)])]))
                      
;; -----------------------------------------------------------------------------
;; determine the standard meaning of b

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
     (second v)]

    [`(fun ,t ,f ,x ,rhs ,body)
     (define c (list x rhs env))
     (value body (cons [list f c] env))]
    [`(cal ,f ,a)
     (define v (assq f env))
     (unless v
       (error 'value "can't happen ~a" f))
     (define c (second v))
     (c-apply c (value a env))]))

(: c-apply (-> Meaning Meaning Meaning))
(define (c-apply c a)
  (match c
    [`(,x ,rhs ,env) (value rhs (cons [list x a] env))]
    [_ (error 'c-apply "can't happen: ~a\n" c)]))

;; -----------------------------------------------------------------------------
;; -----------------------------------------------------------------------------
;; integration and unit tests

(module+ test ;; integration tests for `main`
  (test main '(#true "^" #false)  #false "and false")
  (test main '(#true "||" #false) #true  "or true")
  (test main '(#true "|" #false)  #false "exn" exn:fail?)
  (test main '(5 "+" 0)  5 "numbers")
  (test main '(((5 "+" 0) "<" 10) "^" (#false "||" #true))  #true "complex")
  (test main '["let" "var" "int" "x" "=" (10 "+" 5)
                     "in"
                     "x"]
        15 "simple let")

  (test main '["let" "var" "int" "y" "=" 42
                     "in"
                     ["let" "var" "int" "x" "=" (10 "+" 5)
                            "in"
                            "y"]]
        42 "nested let")

  (test main '["let" "var" "int" "y" "=" 42
                     "in"
                     ["let" "var" "int" "x" "=" (10 "+" 5)
                            "in"
                            ["y" "+" "x"]]]
        57 "nested let")

  (test main '["let" "var" "int" "x" "=" 5
                     "in"
                     ["let" "fun" "int" "f" ["int" "y"] ["x" "+" "y"]
                            "in"
                            ["let" "var" "int" "x" "=" 42
                                   "in"
                                   ["call" "f" 10 ]]]]
        15
        "nested function def"))

(module+ test ;; unit tests for `parse`
  (check-equal? (parse '(#true "^" #false))  '[&& #t #f] "parse and false")
  (check-equal? (parse '(#true "||" #false)) '[|| #t #f] "parse or true")
  (check-exn exn:fail? (λ () (parse '(#true "|" #false))) "exn"))
