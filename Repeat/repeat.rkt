#! /bin/sh
#| -*- racket -*-
exec racket -tm "$0" -- ${1+"$@"}
|#
#lang typed/racket

(define LANGUAGE-NAME "REPEAT JSExpr")

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
    (set! *max-env-size 0)
    ((inst call-with-limits Meaning) 1 10 (λ () (value ast)))))

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
                      [List '== E-AST E-AST]

                      [List 'let (U ERR Type) Symbol E-AST E-AST]
                      [List 'set Symbol E-AST]
                      Symbol

                      [List 'fun (U ERR Type) Symbol Symbol E-AST E-AST]
                      [List 'cal Symbol E-AST]

                      [List 'iff (U ERR E-AST) (U ERR E-AST) (U ERR E-AST)]
                      [List 'rpt (U ERR E-AST) (U ERR E-AST)]
                      
                      ERR))

(define-type AST (U Boolean [List '&& AST AST] [List '|| AST AST]
                    [List '<< AST AST] [List '== AST AST]
                    Integer [List '++ AST AST] [List '-- AST AST]
                    [List 'let (U ERR Type) Symbol AST AST]
                    [List 'set Symbol AST]
                    Symbol
                    [List 'fun Type Symbol Symbol AST AST]
                    [List 'cal Symbol AST]

                    [List 'iff AST AST AST]
                    [List 'rpt AST AST]))

(: parse (JSExpr -> AST))
(define (parse j)
  (define q (e-parse j))
  (with-handlers ([exn:fail? (λ _ (error 'parse "~a expected, given ~e" LANGUAGE-NAME q))])
    (cast q AST)))

(: e-parse [JSExpr -> E-AST])
(define (e-parse j)
  (match j
    [(? boolean?) j]
    [(list j-left "||" j-right) (list '|| (e-parse j-left) (e-parse j-right))]
    [(list j-left "^"  j-right) (list '&& (e-parse j-left) (e-parse j-right))]
    [(list j-left "+"  j-right) (list '++ (e-parse j-left) (e-parse j-right))]
    [(? exact-integer?) j]
    [(list j-left "+"  j-right) (list '++ (e-parse j-left) (e-parse j-right))]
    [(list j-left "-"  j-right) (list '-- (e-parse j-left) (e-parse j-right))]
    ;; mixed arithmetic 
    [(list j-left "<"  j-right) (list '<< (e-parse j-left) (e-parse j-right))]
    [(list j-left "==" j-right) (list '== (e-parse j-left) (e-parse j-right))]
    
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

    [(list "if" condition then "else" else)
     (list 'iff (e-parse condition) (e-parse then) (e-parse else))]

    [(list "repeat" body "until" condition)
     (list 'rpt (e-parse condition) (e-parse body))]
    
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
    [`(,(or '|| '&&) ,left  ,right) (tc-primitive (tc left env) (tc right env) 'bool 'bool 'bool)]
    [(? exact-integer? b) 'int]
    [`(,(or '++ '--) ,left  ,right) (tc-primitive (tc left env) (tc right env) 'int 'int 'int)]
    [`(,(or '<< '==) ,left  ,right) (tc-primitive (tc left env) (tc right env) 'int 'int 'bool)]

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
          (pretty-print b (current-error-port))
          (error 'tc "argument types don't match: ~a vs ~a\n" p-type a-type))
        r-type]
       [_ (error 'tc "function undefined: ~a\n" f)])]

    [`(rpt ,condition ,body)
     (unless (eq? 'bool (tc condition env))
       (error 'tc "boolean expected: ~a\n" b))
     (tc body env)]
    [`(iff ,condition ,then ,else)
     (unless (eq? 'bool (tc condition env))
       (error 'tc "boolean expected: ~a\n" b))
     (define t (tc then env))
     (unless (equal? t (tc else env))
       (error 'tc "same type expected: ~a vs ~a\n" then else))
     t]))

(: tc-primitive (Type Type Type Type Type -> Type))
(define (tc-primitive t-left t-right e-left e-right out)
  (match* (t-left t-right)
    [((== e-left) (== e-right)) out]
    [(_ _) (error 'tc "&& or ||")]))

;                                     
;                 ;;;                 
;                   ;                 
;                   ;                 
;   ;   ;   ;;;     ;    ;   ;   ;;;  
;   ;; ;;  ;   ;    ;    ;   ;  ;   ; 
;    ; ;    ;;;;    ;    ;   ;  ;;;;; 
;    ; ;   ;   ;    ;    ;   ;  ;     
;    ;;;   ;   ;    ;    ;   ;  ;     
;     ;     ;;;;     ;;   ;;;;   ;;;; 
;                                     
;                                     
;                                     

;; determine the standard meaning of b

(define-syntax-rule (bcast e) (cast e Boolean))
(define-syntax-rule (icast e) (cast e Integer))
(define-syntax-rule (ccast e) (cast e Closure))

(: value (->* (AST) (MEnv) Meaning))
(define (value b [env [(inst new-env [Boxof Meaning])]])
  (let value ([b b] [env env])
    (record-size-of-env env)
    (match b
      [(? exact-integer? b) b]

      [(? boolean? b) b]
      [`(&& ,left  ,right) (and (bcast (value left env)) (bcast (value right env)))]
      [`(|| ,left ,right)  (or  (bcast (value left env)) (bcast (value right env)))]

      [(? exact-integer? b) b]
      [`(++ ,left  ,right) (+ (icast (value left env)) (icast (value right env)))]
      [`(-- ,left  ,right) (- (icast (value left env)) (icast (value right env)))]
      [`(<< ,left  ,right) (< (icast (value left env)) (icast (value right env)))]
      [`(== ,left  ,right) (= (icast (value left env)) (icast (value right env)))]

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
       (closure-apply (ccast c) (value a env))]

      [`(rpt ,condition ,body)
       (define x (value body env))
       (if (value condition env) x (value b env))]
      [`(iff ,condition ,then ,else)
       (if (value condition env) (value then env) (value else env))])))

(define *max-env-size 0)
(: record-size-of-env (-> MEnv Void))
(define (record-size-of-env e)
  (set! *max-env-size (max *max-env-size (length e))))

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

;                                                                 
;                                                           ;     
;   ;;;;;                                                   ;     
;   ;                                                       ;     
;   ;       ;;;;   ;;;    ;;;    ;;;  ;     ;  ;;;    ;;;;  ;  ;; 
;   ;;;;    ;  ;  ;   ;   ;;;   ;   ;  ;    ; ;   ;   ;  ;  ; ;;  
;   ;       ;      ;;;;   ;;;   ;;;;;  ; ; ;  ;   ;   ;     ;;;   
;   ;       ;     ;   ;   ;;;   ;      ;;; ;  ;   ;   ;     ; ;   
;   ;       ;     ;   ;   ;;;   ;       ; ;   ;   ;   ;     ;  ;  
;   ;       ;      ;;;;   ;;;    ;;;;   ; ;    ;;;    ;     ;   ; 
;                                                                 
;                                                                 
;                                                                 

(module+ test
  (define-type Expected (U Integer Boolean String (Any -> Any)))

  (define-syntax-rule (translate/test exp msg)
    (let* ([t : JSExpr (translate 'exp)]
           [v : Expected
              (with-handlers ([exn:fail:resource? (λ (xn) INF)])
                (cast (call-with-limits 1 10 (λ () exp)) Expected))])
      (test main t (value->expected v) msg)
      (printf "~a :: max env size ~a\n" msg *max-env-size)))

  (: value->expected (-> Expected Expected))
  (define (value->expected v)
    (if (procedure? v) "closure" v))
  
  (: translate (Sexp -> JSExpr))
  (define (translate r)
    (match r
      [`(let ([,x : ,t ,rhs]) ,body)
       `["let" "var" ,(translate-type t) ,(~a x) "=" ,(translate rhs)
               "in" ,(translate body)]]
      [`(letrec ([,f : (,at -> ,rt) (λ (,x) ,rhs)]) ,body)
       `["let" "fun" ,(translate-type rt) ,(~a f) [,(translate-type at) ,(~a x)] ,(translate rhs)
               "in" ,(translate body)]]
      [`(if ,c ,t ,e)
       `["if" ,(translate c) ,(translate t) "else" ,(translate e)]]
      [`(begin0 ,(? symbol? x) (set! ,(? symbol? y) ,rhs))
       (unless (eq? x y) (error 't ""))
       `[,(~a x) "=" ,(translate rhs)]]
      [`(,o ,left ,right)
       `[,(translate left) ,(translate-primitive o) ,(translate right)]]
      [(or (? integer? c) (? boolean? c))  c]
      ((? symbol? x) (~a x))
      [`(,f ,a)
       `["call" ,(~a f) ,(translate a)]]
      [_ (error 't "~a\n" r)]))

  (define (translate-primitive o)
    (match o
      [(== '=) "=="]
      [(== '-) "-"]
      [(== '^) "^"]
      [(== '+) "+"]
      [(== '<) "<"]
      [(== '||) "||"]
      
      [_ (error 'tp "")]))

  (: translate-type (Sexp -> JSExpr))
  (define (translate-type t)
    (match t
      ['Integer "int"]
      ['Boolean "bool"]
      [`[,d -> ,r] `[,(translate-type d) "->" ,(translate-type r)]])))

;                                     
;                                     
;  ;;;;;;;                ;           
;     ;                   ;           
;     ;     ;;;    ;;;; ;;;;;    ;;;; 
;     ;    ;   ;  ;       ;     ;     
;     ;    ;;;;;  ;;;     ;     ;;;   
;     ;    ;         ;;   ;        ;; 
;     ;    ;          ;   ;         ; 
;     ;     ;;;;  ;;;;    ;;;   ;;;;  
;                                     
;                                     
;                                     

(module+ test ;; failing test
  (test main '(#true "|" #false)  #false "exn" exn:fail?))

(module+ test ;; integration tests for repeat, if 
  (translate/test
   (let ([x : Integer 100])
     (letrec ([body : (Boolean -> Integer) (λ (_) (begin0 x (set! x (- x 1))))])
       (letrec ([condition : (Boolean -> Boolean) (λ (_) (= x 0))])
         (letrec ([repeat : (Boolean -> Integer)
                          (λ (_)
                            (let ([x : Integer (body #t)])
                              (if (condition #t) x (repeat #t))))])
           (repeat #true)))))
   "repeat as function test"))

(module+ test
  (test main
        '["let" "var" "int" "x" "=" 10 "in"
                ["repeat"
                 ["x" "=" ["x" "-" 1]]
                 "until"
                 ["x" "==" 0]]]
        1
        "repeat as loop test")
  (printf "~a :: max env size ~a\n" "repeat as loop test" *max-env-size))

(module+ test
  (define-syntax-rule (^ x y) (and x y))
  (define-syntax-rule (|| x y) (or x y))

  (translate/test (^ #true #false) "and false")
  (translate/test (|| #true #false) "or true")
  (translate/test (+ 5 6) "numbers")
  (translate/test (< (+ 5 6) 10) "complex expr")
  (translate/test
   (let ([x : Integer (+ 10 5)])
     x)
   "simple let")
  (translate/test
   (let ([y : Integer 42])
     (let ([x : Integer (+ 10 5)])
       y))
   "nested let")

  (translate/test
   (let ([y : Integer 42])
     (let ([x : Integer (+ 10 5)])
       (+ y x)))
   "nested let, again")

  (translate/test
   (let ([x : Integer 5])
     (letrec ([f : (Integer -> Integer) (λ (y) (+ x y))])
       (let ([x : Integer 42])
         (f 10))))
   "nested function def")
  
  (translate/test
   (let ([x : Integer 5])
     (letrec ([f : (Integer -> Integer) (λ (y) (+ x y))])
       (let ([x : Integer 42])
         (f x))))
   "nested fun def, static vs dynamic scope")

  (translate/test
   (let ([x : Integer 5])
     (letrec ([f : (Integer -> Integer) (λ (y) (+ x y))])
       (let ([x : Integer 42])
         (f (begin0 x (set! x (+ x 1)))))))
   "useless assignment statement")

  (translate/test
   (let ([x : Integer 5])
     (letrec ([f : (Integer -> Integer) (λ (y) (+ x y))])
       (let ([x : Integer 42])
         (f (let ([_ : Integer (begin0 x (set! x (+ x 1)))])
              x)))))
  
   "assignment statement used")

  (translate/test
   (let ([x : Integer 5])
     (letrec ([f : (Integer -> Integer)
                 (λ (y)
                   (let ([z : Integer (begin0 x (set! x (+ x 2)))])
                     (+ x y)))])
       (let ([x : Integer 42])
         (f (let ([_ : Integer (begin0 x (set! x (+ x 1)))])
              x)))))
   "assignment statement in fun def to closed-over var")

  (translate/test
   (let ([x : Integer 5])
     (letrec ([f : (Integer -> Integer)
                 (λ (y)
                   (let ([z : Integer (begin0 y (set! y 0))])
                     (+ x y)))])
       (let ([x : Integer 42])
         (f x))))
   "set parameter to 0")

  (translate/test
   (let ([x : Integer 5])
     (letrec ([f : (Integer -> Integer)
                 (λ (y)
                   (let ([z : Integer (begin0 y (set! y 0))])
                     (+ x y)))])
       (let ([x : Integer 42])
         (let ([z : Integer (f x)])
           (+ x z)))))
   "set parameter to 0, and refer to passed-in variable")

  ;; ----------------------------------------------------------------------------------------
  ;; functions as values
  
  (translate/test
   (let ([x : Integer 1])
     (letrec ([g : (Integer -> Integer) (λ (y) x)])
       (letrec ([f : (Integer -> (Integer -> Integer)) (λ (y) g)])
         f)))
   "ho 1")

  (translate/test
   (let ([x : Integer 1])
     (letrec ([g : (Integer -> Integer) (λ (y) x)])
       (letrec ([f : ((Integer -> Integer)  -> Integer) (λ (y) 0)])
         f)))
   "ho 2")


  ;; ------------------------------------------------------------------------------------------------
  ;; a function for setting an int var to a new value
  (translate/test
   (let ([x : Integer 1])
     (letrec ([setx : (Integer -> Integer) (λ (nu) (begin0 x (set! x nu)))])
       (let ([_ : Integer (setx 2)])
         x)))
   "int swap")

  ;; a functionn for setting a bool var to a new value
  (translate/test
   (let ([x : Boolean #true])
     (letrec ([setx : (Boolean -> Boolean) (λ (nu) (begin0 x (set! x nu)))])
       (let ([_ : Boolean (setx #false)])
         x)))
   "boolean swap"))

(module+ test ;; solution to homework problem
  ;; the first infinite loop
  (translate/test
   (letrec ([f : (Integer -> Integer) (λ (a) 0)])
     (let ([_ : (Integer -> Integer)
              (begin0 f
                      (set! f (letrec ([g : (Integer -> Integer) (λ (x) (f 0))]) g)))])
       (f 0)))
   "set inf loop")
  
  ;; the second infinite loop
  (translate/test
   (letrec ([f : (Integer -> Integer) (λ (x) (f x))])
     (f 0))
   "inf loop"))