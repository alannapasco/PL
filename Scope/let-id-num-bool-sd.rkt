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
  (define lt# : [Boxof Natural] (box 0))
  
  (define ast (parse input lt#))
  (define ___ (tc ast))
  (define out (value ast))
  
  (define ast-sd (static-distance ast))
  (define out-sd (value2 ast-sd (unbox lt#)))

  (unless (equal? out-sd out)
    (error 'pipe "should not happen"))

  out)

;; -----------------------------------------------------------------------------
;; parsing JSExpr to make sure they are Boolean JSExpr

(struct ERR ([x : Any]))
(struct sd ([depth : Natural]) #:type-name SD)

(define-type Type (U 'int 'bool))

(define-type AST (U Boolean [List '&& AST AST] [List '|| AST AST]
                    Integer [List '++ AST AST] [List '-- AST AST]
                    [List 'let (U ERR Type) Symbol AST AST]
                    Symbol
                    SD
                    [List '<< AST AST] ))

(define-type E-AST (U Boolean
                      [List '&& E-AST E-AST]
                      [List '|| E-AST E-AST]

                      Integer
                      [List '++ E-AST E-AST]
                      [List '-- E-AST E-AST]
                      [List '<< E-AST E-AST]

                      [List 'let (U ERR Type) Symbol E-AST E-AST]
                      Symbol
                      SD
                      
                      ERR))

(: parse (JSExpr [Boxof Natural] -> AST))
(define (parse j lt#)
  (define q (e-parse j lt#))
  (with-handlers ([exn:fail? (λ _ (error 'parse "JSExpr expected, given ~e" q))])
    (cast q AST)))

(: e-parse [JSExpr [Boxof Natural] -> E-AST])
(define (e-parse j0 lt#)
  (let e-parse ([j j0])
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
       (set-box! lt# (add1 (unbox lt#)))
       (list 'let (t-parse j-type) (string->symbol x) (e-parse j-rhs) (e-parse j-body))]
      [(? string? id) (string->symbol id)]
    
      [_ (ERR j)])))

(: t-parse (JSExpr -> (U ERR Type)))
(define (t-parse j)
  (match j
    ["int" 'int]
    ["bool" 'bool]
    [_ (ERR j)]))

;; -----------------------------------------------------------------------------
;; replace varoable occurrences with the static distance to their declaration 

(: static-distance (AST -> AST))
(define (static-distance ast0)
  (let static-distance ([ast ast0] [env : [Listof Symbol] '()])
    (match ast
      [(? sd? s) (error 'tc "can't happen")]
      [(? boolean? b) b]
      [`(++ ,left  ,right) (list '++ (static-distance left env) (static-distance right env))]
      [`(-- ,left  ,right) (list '-- (static-distance left env) (static-distance right env))]
      [`(<< ,left  ,right) (list '<< (static-distance left env) (static-distance right env))]
      [`(&& ,left  ,right) (list '&& (static-distance left env) (static-distance right env))]
      [`(|| ,left  ,right) (list '|| (static-distance left env) (static-distance right env))]
      [(? exact-integer? b) b]
      [`(let ,t ,x ,rhs ,body)
       `(let ,t ,x ,(static-distance rhs env)
          ,(static-distance body ((inst cons Symbol Symbol) x env)))]
      [(? symbol? x) 
       (define t (index-of env x))
       (unless t
         (error 'static-distance "can't happen: ~a\n" ast))
       (sd t)])))
  
;; -----------------------------------------------------------------------------
;; type check AST 

(define-type TEnv [Listof [List Symbol Type]])

(: tc (AST -> Type))
(define (tc ast0)
  (let tc ([ast ast0] [env : TEnv '()])
    (match ast
      [(? sd? s) (error 'tc "can't happen")]
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
         (error 'tc "types don't match: ~a ~a \n ~a\n" t t-rhs ast))
       (tc body (cons [list x t] env))]
      [(? symbol? x)
       (define t (assq x env))
       (unless t
         (error 'tc "identifier undefined: ~a\n" ast))
       (second t)])))
                      
;; -----------------------------------------------------------------------------
;; determine the standard meaning of an expression

(define-type Env (Listof [List Symbol Meaning]))

(define-syntax-rule (bcast e) (cast e Boolean))
(define-syntax-rule (icast e) (cast e Integer))

(: value (-> AST Meaning))
(define (value ast0)
  (let value ([ast ast0] [env : Env '()])
    (match ast
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
       (second v)])))

;; -----------------------------------------------------------------------------
;; a stack based implementation of `value`

(define stack%
  (class object% [init (n : Natural)]

    (define s : (Vectorof Meaning) (make-vector n #false))
    (define top : Integer -1)

    (define/public (push {m : Meaning})
      (set! top (add1 top))
      (vector-set! s top m))

    (define/public (pop)
      (set! top (sub1 top)))

    (define/public (ref {i : Natural})
      (vector-ref s (- top i)))
      
    (super-new)))

(: value2 (-> AST Natural Meaning))
(define (value2 ast0 n)
  (define stack [new stack% [n n]])
  (let value ([ast ast0])
    (match ast
      [(? exact-integer? b) b]
      [(? boolean? b) b]
      [`(&& ,left  ,right) (and (bcast (value left)) (bcast (value right)))]
      [`(|| ,left ,right)  (or  (bcast (value left)) (bcast (value right)))]
      [(? exact-integer? b) b]
      [`(++ ,left  ,right) (+ (icast (value left)) (icast (value right)))]
      [`(-- ,left  ,right) (- (icast (value left)) (icast (value right)))]
      [`(<< ,left  ,right) (< (icast (value left)) (icast (value right)))]
      [`(let ,t ,x ,rhs ,body)
       (send stack push (value rhs))
       (begin0
         (value body)
         (send stack pop))]
      [(sd x)
       (define v (send stack ref x))
       (unless v
         (error 'value "can't happen ~a" x))
       v]
      [argh (error 'vale2 "can't happen ~a [~a]" ast0 argh)])))

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
        57 "nested let")

  (test main '["let" "int" "x" "=" (10 "+" 5)
                     "in"
                     [["let" "bool" "x" "=" #true
                             "in"
                             "x"]
                      "^"
                      ["x" "<" 16]]]
        #t "shadowing let"))

(module+ test ;; unit tests for `parse`
  (check-equal? (parse '(#true "^" #false) [box 0])  '[&& #t #f] "parse and false")
  (check-equal? (parse '(#true "||" #false) [box 0]) '[|| #t #f] "parse or true")
  (check-exn exn:fail? (λ () (parse '(#true "|" #false) [box 0])) "exn"))
