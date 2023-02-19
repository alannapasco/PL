#! /bin/sh
#| -*- racket -*-
exec racket -tm "$0" -- ${1+"$@"}
|#
#lang typed/racket

(provide main)

;; -----------------------------------------------------------------------------
(require typed/json)
(require/typed
 racket/sandbox
 [exn:fail:resource? (-> Any Boolean)]
 [call-with-limits   (∀ (α) (Natural Natural (-> α) -> α))])
(require "../../Git/Lib/environments.rkt")
(module+ test
  (require "../../Git/Lib/test-framework.rkt")
  (require typed/rackunit))

;; -----------------------------------------------------------------------------
;; interpret a JSON representation

(define (main)
  (define input  : (U EOF JSExpr)  (read-json))
  (cond [(eof-object? input) (void)] [else (write-json (pipe input)) (newline)]))

(define-type Meaning (U Boolean Integer Closure))
[struct closure [{param : Symbol} {body : AST} {env : MEnv}] #:mutable #:type-name Closure]
; (define-type Closure [List Symbol AST MEnv])
(define-type MEnv    [Env (Boxof Meaning)])

(: pipe (JSExpr -> JSExpr))
(define (pipe input)
  (define ast (parse input))
  (define ___ (tc ast '[]))
  (define out (timed-value ast))
  (meaning->jsexpr out))

(: timed-value {AST -> (U String Meaning)})
(define (timed-value ast)
  (with-handlers ([exn:fail:resource? (λ (xn) INF)])
    ((inst call-with-limits Meaning) 1 10 (λ () (value ast [(inst new-env [Boxof Meaning])])))))

(: meaning->jsexpr (-> (U String Meaning) JSExpr))
(define (meaning->jsexpr m)
  (match m
    [(? boolean?) m]
    [(? integer?) m]
    [(? string?) m]
    [_ "closure"]))

(define INF "infinite loop")

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
                      [List 'set Symbol E-AST]
                      Symbol

                      [List 'fun (U ERR Type) Symbol Symbol E-AST E-AST]
                      [List 'cal Symbol E-AST]
                      
                      ERR))

(define-type AST (U Boolean [List '&& AST AST] [List '|| AST AST] [List '<< AST AST]
                    Integer [List '++ AST AST] [List '-- AST AST]
                    [List 'let (U ERR Type) Symbol AST AST]
                    [List 'set Symbol AST]
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
    [(list (? string? x) "=" j-rhs)
     (list 'set (string->symbol x) (e-parse j-rhs))]
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
    [(list d "->" r)
     (define dt (t-parse d))
     (cond
       [(ERR? dt) (ERR j)]
       [else (define rt (t-parse r))
             (cond
               [(ERR? rt) (ERR j)]
               [else (list '-> dt rt)])])]
    [_ (ERR j)]))

;; -----------------------------------------------------------------------------
;; type check AST 

(define-type TEnv [Env Type])

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
     (tc body (env-add env x t))]
    [`(set ,x ,rhs)
     (define t-x   (env-retrieve env x (~a x " : undefined variable")))
     (define t-rhs (tc rhs env))
     (unless (equal? t-x t-rhs)
       (error 'tc "types don't match: ~a ~a \n ~a\n" t-x t-rhs b))
     t-x]
     
    [(? symbol? x) (env-retrieve env x (~a x " : undefined variable"))]

    [`(fun ,(and f-type `(-> ,p-type ,r-type)) ,f ,a ,rhs ,body)
     (define t-rhs (tc rhs (env-add (env-add env a p-type) f f-type)))
     (unless (equal? t-rhs r-type)
       (error 'tc "return types don't match: ~a \n ~a\n" t-rhs p-type))
     (tc body (env-add env f `(-> ,p-type ,r-type)))]
    [`(cal ,f ,a)
     (define t (env-retrieve env f (~a "function undefined: ~a \n  in" env "\n")))
     (match t
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
(define-syntax-rule (ccast e) (cast e Closure))

(: value (-> AST MEnv Meaning))
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

    [`(let ,t ,x ,rhs ,body) (value body (env-add env x [box (value rhs env)]))]
    [`(set ,x ,rhs)
     (define m (env-retrieve env x "can't happen (variable undefined) ~a"))
     (define o (unbox m))
     (set-box! m (value rhs env))
     o]
    [(? symbol? x)
     (unbox (env-retrieve env x "can't happen (variable undefined) ~a"))]

    [`(fun ,t ,f ,x ,rhs ,body)
     (value body (create-rec-env f x rhs env))]
    [`(cal ,f ,a)
     (define v (env-retrieve env f "can't happen (function undefined) ~a"))
     (define c (unbox v))
     (closure-apply (ccast c) (value a env))]))

(: create-rec-env (-> Symbol Symbol AST MEnv MEnv))
;; creates a cyclic environment at `f` through `[closure x rhs .]` 
(define (create-rec-env f x rhs env)
  (define b : [Boxof Meaning] (box (closure x rhs env)))
  (define e (env-add env f b))
  (set-box! b (closure x rhs e))
  e)

(: closure-apply (-> Closure Meaning Meaning))
(define (closure-apply c a)
  (match c
    [(closure x rhs env) (value rhs (env-add env x (box a)))]
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
        "nested function def")
  
  (test main '["let" "var" "int" "x" "=" 5
                     "in"
                     ["let" "fun" "int" "f" ["int" "y"] ["x" "+" "y"]
                            "in"
                            ["let" "var" "int" "x" "=" 42
                                   "in"
                                   ["call" "f" "x" ]]]]
        47
        "nested fun def, static vs dynamic scope")

  (test main '["let" "var" "int" "x" "=" 5
                     "in"
                     ["let" "fun" "int" "f" ["int" "y"] ["x" "+" "y"]
                            "in"
                            ["let" "var" "int" "x" "=" 42
                                   "in"
                                   ["call" "f" ["x" "=" ["x" "+" 1]]]]]]
        47
        "useless assignment statement")

  (test main '["let" "var" "int" "x" "=" 5
                     "in"
                     ["let" "fun" "int" "f" ["int" "y"] ["x" "+" "y"]
                            "in"
                            ["let" "var" "int" "x" "=" 42
                                   "in"
                                   ["call" "f"
                                           ["let" "var" "int" "_" "=" ["x" "=" ["x" "+" 1]]
                                                  "in"
                                                  "x"]]]]]
        48
        "assignment statement used")

  (test main '["let" "var" "int" "x" "=" 5
                     "in"
                     ["let" "fun" "int" "f" ["int" "y"]
                            ["let" "var" "int" "z" "=" ["x" "=" ["x" "+" 2]]
                                   "in" 
                                   ["x" "+" "y"]]
                            "in"
                            ["let" "var" "int" "x" "=" 42
                                   "in"
                                   ["call" "f"
                                           ["let" "var" "int" "_" "=" ["x" "=" ["x" "+" 1]]
                                                  "in"
                                                  "x"]]]]]
        50
        "assignment statement in fun def to closed-over var")

  (test main '["let" "var" "int" "x" "=" 5
                     "in"
                     ["let" "fun" "int" "f" ["int" "y"]
                            ["let" "var" "int" "z" "=" ["y" "=" 0]
                                   "in"
                                   ["x" "+" "y"]]
                            "in"
                            ["let" "var" "int" "x" "=" 42
                                   "in"
                                   ["call" "f" "x" ]]]]
        5
        "set parameter to 0")

  (test main '["let" "var" "int" "x" "=" 5
                     "in"
                     ["let" "fun" "int" "f" ["int" "y"]
                            ["let" "var" "int" "z" "=" ["y" "=" 0]
                                   "in"
                                   ["x" "+" "y"]]
                            "in"
                            ["let" "var" "int" "x" "=" 42
                                   "in"
                                   ["let" "var" "int" "z" "=" ["call" "f" "x"]
                                          "in"
                                          ["z" "+" "x"]]]]]
        47
        "set parameter to 0")

  ;; ----------------------------------------------------------------------------------------
  ;; functions as values
  
  (test main 
        '["let" "var" "int" "x" "=" 1 "in" 
                ["let" "fun" "int" "g" ["int" "y"] "x" "in" 
                       ["let" "fun" ["int" "->" "int"] "f" ["int" "y"] "g" "in" 
                              "f"]]]
        "closure"
        "ho 1")

  (test main 
        '["let" "var" "int" "x" "=" 1 "in" 
                ["let" "fun" "int" "g" ["int" "y"] "x" "in" 
                       ["let" "fun" "int" "f" [["int" "->" "int"] "y"] 0 "in" 
                              "f"]]]
        "closure"
        "ho 2")


  ;; ------------------------------------------------------------------------------------------------
  ;; a function for setting an int var to a new value
  (test main '["let" "var" "int" "x" "=" 1 "in" 
                     ["let" "fun" "int" "setx" ["int" "nu"] ["x" "=" "nu"] "in" 
                            ["let" "var" "int" "_" "=" ["call" "setx" 2] "in" 
                                   "x"]]]
        2
        "int swap")

  ;; a functionn for setting a bool var to a new value
  (test main '["let" "var" "bool" "x" "=" #true "in" 
                     ["let" "fun" "bool" "setx" ["bool" "nu"] ["x" "=" "nu"] "in" 
                            ["let" "var" "bool" "_" "=" ["call" "setx" #false] "in" 
                                   "x"]]]
        #false
        "boolean swap"))

#;
(module+ test ;; solution to homework problem
  ;; the first infinite loop
  (test main 
        '["let" "fun" "int" "f" ["int" "a"] 0
                "in" 
                ["let" "var" ["int" "->" "int"] "_" "=" 
                       ["f" "="
                            ["let" "fun" "int" "g" ["int" "x"] ["call" "f" 0] "in" "g"]]
                       "in"
                       ["call" "f" 0]]]
        INF
        "set inf loop")
  
  ;; the second infinite loop
  (test main 
        '["let" "fun" "int" "f" ["int" "x"] ["call" "f" "x"] "in" ["call" "f" 0]]
        INF
        "inf loop")
  )