#! /bin/sh
#| -*- racket -*-
exec racket -tm "$0" -- ${1+"$@"}
|#
#lang typed/racket

(define LANGUAGE-NAME "Forall JSExpr")

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

(define-type Meaning (U Boolean Integer Closure (-> Any Meaning)))
[struct closure [{param : Symbol} {body : AST} {env : MEnv}] #:mutable #:type-name Closure]
; (define-type Closure [List Symbol AST MEnv])
(define-type MEnv    [Env (Boxof Meaning)])

(: pipe (JSExpr -> JSExpr))
(define (pipe input)
  (define ast (parse input))
  (define ___ (tc ast [(inst new-env Type)]))
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

;                                     
;                                     
;                                     
;                                     
;   ;;;;    ;;;    ;;;;   ;;;;   ;;;  
;   ;   ;  ;   ;   ;  ;  ;      ;   ; 
;   ;   ;   ;;;;   ;     ;;;    ;;;;; 
;   ;   ;  ;   ;   ;        ;;  ;     
;   ;   ;  ;   ;   ;         ;  ;     
;   ;;;;    ;;;;   ;     ;;;;    ;;;; 
;   ;                                 
;   ;                                 
;                                     

(struct ERR ([x : Any]) #:transparent)

(define-type Type (U 'int 'bool (List '-> Type Type) (List '∀ Symbol Type) Symbol))

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
                      [List 'cal E-AST E-AST]

                      [List 'iff (U ERR E-AST) (U ERR E-AST) (U ERR E-AST)]
                      [List 'rpt (U ERR E-AST) (U ERR E-AST)]

                      [List 'tfun (U ERR Type) Symbol Symbol E-AST E-AST]
                      [List 'tcal E-AST (U ERR Type)]
                      
                      ERR))

(define-type AST (U Boolean [List '&& AST AST] [List '|| AST AST]
                    [List '<< AST AST] [List '== AST AST]
                    Integer [List '++ AST AST] [List '-- AST AST]
                    [List 'let Type Symbol AST AST]
                    [List 'set Symbol AST]
                    Symbol
                    [List 'fun Type Symbol Symbol AST AST]
                    [List 'cal AST AST]

                    [List 'iff AST AST AST]
                    [List 'rpt AST AST]
                    
                    [List 'tfun Type Symbol Symbol AST AST]
                    [List 'tcal AST Type]))

(: parse (JSExpr -> AST))
(define (parse j)
  (define q (e-parse j))
  (with-handlers ([exn:fail? (λ _
                               (pretty-print q (current-error-port))
                               (error 'parse "~a expected, given ~e" LANGUAGE-NAME q))])
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
    [(list "call" f j-arg)
     (list 'cal (e-parse f) (e-parse j-arg))]

    [(list "if" condition then "else" else)
     (list 'iff (e-parse condition) (e-parse then) (e-parse else))]

    [(list "repeat" body "until" condition)
     (list 'rpt (e-parse condition) (e-parse body))]

    [(list "let" "tfun" `(,[? string? α]) rj-type (? string? f) `[,aj-type ,(? string? x)] j-rhs "in"
           j-body)
     (define ->type (forall-type (string->symbol α) (fun-type (t-parse aj-type) (t-parse rj-type))))
     (list 'tfun ->type (string->symbol f) (string->symbol x) (e-parse j-rhs) (e-parse j-body))]
    [(list "tcall" f j-arg)
     (list 'tcal (e-parse f) (t-parse j-arg))]
    
    [_ (ERR j)]))

(: fun-type ((U ERR Type) (U ERR Type) -> (U ERR (List '-> Type Type))))
(define (fun-type a-type r-type)
  (cond
    [(ERR? a-type) (ERR "bad arg type")]
    [(ERR? r-type) (ERR "bad return type")]
    [else (list '-> a-type r-type)]))

(: forall-type (Symbol (U ERR Type) -> (U ERR Type)))
(define (forall-type tvar type)
  (cond
    [(ERR? type) (ERR (~a "bad t-return type (quantified " tvar))]
    [else (list '∀ tvar type)]))

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
    [(? string?) (string->symbol j)]
    [_ (ERR j)]))

;                                                                        
;                                             ;                    ;     
;    ;                                        ;                    ;     
;    ;                                        ;                    ;     
;  ;;;;;   ;   ;  ;;;;    ;;;           ;;;;  ; ;;    ;;;    ;;;;  ;  ;; 
;    ;     ;; ;   ;   ;  ;   ;         ;;     ;;  ;  ;   ;  ;;     ; ;;  
;    ;      ; ;   ;   ;  ;;;;;         ;      ;   ;  ;;;;;  ;      ;;;   
;    ;      ; ;   ;   ;  ;             ;      ;   ;  ;      ;      ; ;   
;    ;      ;;    ;   ;  ;             ;;     ;   ;  ;      ;;     ;  ;  
;    ;;;     ;    ;;;;    ;;;;          ;;;;  ;   ;   ;;;;   ;;;;  ;   ; 
;           ;     ;                                                      
;          ;;     ;                                                      
;                                                                        

(define-type TEnv [Env Type])

(: tc (AST TEnv -> Type))
(define (tc b env)
  (match b
    [(? boolean? b) 'bool]
    [`(,(or '|| '&&) ,left  ,right) (tc-primitive (tc left env) (tc right env) 'bool 'bool 'bool)]
    [(? exact-integer? b) 'int]
    [`(,(or '++ '--) ,left  ,right) (tc-primitive (tc left env) (tc right env) 'int 'int 'int)]
    [`(,(or '<< '==) ,left  ,right) (tc-primitive (tc left env) (tc right env) 'int 'int 'bool)]

    [`(let ,t ,x ,rhs ,body) (compare b t (tc rhs env) "let var" (tc body (env-add env x t)))]
    [`(set ,x ,rhs) (compare b (env-retrieve env x "=") (tc rhs env) "set")]
    [(? symbol? x) (env-retrieve env x "undefined variable")]

    [`(fun ,(and f-type `(-> ,p-type ,r-type)) ,f ,a ,rhs ,body)
     (define env+f (env-add env f f-type))
     (compare b r-type (tc rhs (env-add env+f a p-type)) "fun return" (tc body  env+f))]
    [`(cal ,f ,a) (f-compare b f (tc f env) (tc a env))]
    [`(rpt ,cond ,body) (compare b 'bool (tc cond env) "stop condition" (tc body env))]
    [`(iff ,condition ,then ,else)
     (compare b 'bool (tc condition env) "if condition")
     (compare b (tc then env) (tc else env) "then & else branch")]
    [`(tcal ,f ,t)
     (define f-type (tc f env))
     (match f-type
       [`(∀ ,α ,t1) (type-substitute t1 α t)]
       [t (error 'tc "not a type-function: ~a\n ~a" f t)])]
    [`(tfun ,(and f-type `(∀ ,α (-> ,p-type ,r-type))) ,f ,x ,rhs ,body)
     (define env+f (env-add env f f-type))
     (compare b r-type (tc rhs (env-add env+f x p-type)) "tfun return" (tc body  env+f))]))

(: f-compare (AST AST Type Type -> Type))
(define (f-compare b f exp-type actual-type)
  (match exp-type
    [`(-> ,p-type ,r-type) (compare b p-type actual-type "argument types" r-type)]
    [_ (error 'tc "not a function: ~a\n" f)]))

(: compare  (->* {AST Type Type String} {Type} Type))
(define (compare b exp-type actual-type fmt [t-res #false])
  (unless (type-compare exp-type actual-type)
    (error 'tc (~a fmt ": exp. type: ~a  vs actual ~a\n in: ~a") exp-type actual-type b))
  [if (false? t-res) exp-type t-res])

(: tc-primitive (Type Type Type Type Type -> Type))
(define (tc-primitive t-left t-right e-left e-right out)
  (match* (t-left t-right)
    [((== e-left) (== e-right)) out]
    [(_ _) (error 'tc "&& or ||")]))

(: type-compare (Type Type -> Boolean))
(define (type-compare s t)
  (match* (s t)
    [(int int) #true]
    [(bool bool) #true]
    [(`(,sd -> ,sr) `(,td -> ,tr)) (and (type-compare sd td) (type-compare sr tr))]
    [((? symbol? s) (? symbol? t)) (eq? s t)]
    [(`(∀ ,α ,s) `(∀ ,β ,t))
     (define γ (gensym))
     (type-compare (raw-substitute s α γ) (raw-substitute t β γ))]
    [(_ _) #false]))

(: type-substitute (Type Symbol Type -> Type))
;; ASSUME α may occure in either `t1` or `t`
(define (type-substitute t1 α t)
  (match t1
    [(or 'int 'bool) t1]
    [`(-> ,d ,r) `(-> ,(type-substitute d α t) ,(type-substitute r α t))]
    [(? symbol?) (if (eq? t1 α) t t1)]
    [`(∀ ,β ,s)
     (define γ (gensym))
     (define t1 (raw-substitute s β γ))
     `(∀ ,γ ,(type-substitute t1 γ t))]))

(: raw-substitute (Type Symbol Type -> Type))
;; ASSUME α occur neither in `t1` not in `t`
(define (raw-substitute t1 α t)
  (match t1
    [(or 'int 'bool) t1]
    [`(-> ,d ,r) `(-> ,(raw-substitute d α t) ,(raw-substitute r α t))]
    [(? symbol?) (if (eq? t1 α) t t1)]
    [`(∀ ,β ,s) `(∀ ,β ,(raw-substitute s α t))]))

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
(define-syntax-rule (tcast e) (cast e (-> Any Meaning)))

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
       (define c (value f env) #;(env-retrieve env f "can't happen (function undefined) ~a"))
       (closure-apply (ccast c) (value a env))]

      [`(rpt ,condition ,body)
       (define x (value body env))
       (if (value condition env) x (value b env))]
      [`(iff ,condition ,then ,else)
       (if (value condition env) (value then env) (value else env))]
      [`(tcal ,f ,t)
       (value f env)]
      [`(tfun ,f-type ,f ,x ,rhs ,body)
       (value body (create-rec-env f x rhs env))])))

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

  (define-syntax (repeat stx)
    (syntax-case stx (until)
      [(_ stmt until e)
       #'(let loop ()
           (define v stmt)
           (if e v (loop)))]
      [(_ stmt : α until e)
       #'(let loop : α ()
           (define v stmt)
           (if e v (loop)))]))
  
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
      [`(let ()
          (: ,f (∀ (,α) ,(and t `(,at -> ,rt))))
          (define (,g {,x : ,s}) ,r)
          ,h)
       (unless (eq? f g) (error 'translate "same names expected: ~a vs ~a" f g))
       (define result (translate-type rt))
       (define paramt (translate-type at))
       `["let" "tfun" [,(~a α)] ,result ,(~a f) [,paramt ,(~a x)] ,(translate r)
               "in" ,(translate h)]]
      [`(inst ,f ,t)
       `["tcall" ,(translate f) ,(translate-type t)]]
      [`(let ([,x : ,t ,rhs]) ,body)
       `["let" "var" ,(translate-type t) ,(~a x) "=" ,(translate rhs)
               "in" ,(translate body)]]
      [`(letrec ([,f : (,at -> ,rt) (λ (,x) ,rhs)]) ,body)
       `["let" "fun" ,(translate-type rt) ,(~a f) [,(translate-type at) ,(~a x)] ,(translate rhs)
               "in" ,(translate body)]]
      [`(let ([,f : (∀ (,α) ,rt) ,rhs]) ,body)
       `["let" "tfun" ,(translate-type rt) ,(~a f) (,(~a α)) ,(translate rhs)
               "in" ,(translate body)]]
      [`(if ,c ,t ,e)
       `["if" ,(translate c) ,(translate t) "else" ,(translate e)]]
      [`(begin0 ,(? symbol? x) (set! ,(? symbol? y) ,rhs))
       (unless (eq? x y) (error 't ""))
       `[,(~a x) "=" ,(translate rhs)]]
      [`(repeat ,stmt : ,α until ,stop)
       `["repeat" ,(translate stmt) "until" ,(translate stop)]]
      [`(repeat ,stmt until ,stop)
       `["repeat" ,(translate stmt) "until" ,(translate stop)]]
      [`(,o ,left ,right)
       `[,(translate left) ,(translate-primitive o) ,(translate right)]]
      [(or (? integer? c) (? boolean? c))  c]
      ((? symbol? x) (~a x))
      [`(,f ,a)
       `["call" ,(translate f) ,(translate a)]]
      [_ (error 't "~a\n" r)]))

  (define (translate-primitive o)
    (match o
      [(== '=) "=="]
      [(== '-) "-"]
      [(== '^) "^"]
      [(== '+) "+"]
      [(== '<) "<"]
      [(== '||) "||"]
      
      [_ (error 'tp "~a" o)]))

  (: translate-type (Sexp -> JSExpr))
  (define (translate-type t)
    (match t
      ['Integer "int"]
      ['Boolean "bool"]
      [(? symbol?) (~a t)]
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

(module+ test ;; repeat, if 
  (translate/test
   (let ([x : Integer 100])
     (letrec ([body : (Boolean -> Integer) (λ (_) (begin0 x (set! x (- x 1))))])
       (letrec ([condition : (Boolean -> Boolean) (λ (_) (= x 0))])
         (letrec ([repeat : (Boolean -> Integer)
                          (λ (_)
                            (let ([x : Integer (body #t)])
                              (if (condition #t) x (repeat #t))))])
           (repeat #true)))))
   "repeat as function test")

  (translate/test 
   [let ([x : Integer 10])
     [repeat (begin0 x [set! x [- x 1]]) : Integer until [= x 0]]]
   "repeat as loop test"))

(module+ test
  (translate/test 
   (let ([start : Integer 42424242424242424])
     ;; silly functions that should be baked in 
     (letrec ([true? : (Boolean -> Boolean) (λ (x) x)])
       (letrec ([not : (Boolean -> Boolean) (λ (x) (if x #false #true))])
         (letrec ([zero? : (Integer -> Boolean) (λ (x) (= x 0))])
           (letrec ([sub1 : (Integer -> Integer) (λ (x) (- x 1))])
             ;; the 'repeated code'
             (letrec ([expt-i : (Integer -> Integer)
                              (λ (x)
                                (repeat (begin0 x (set! x (sub1 x))) until (zero? x)))])
               (letrec ([expt-b : (Boolean -> Boolean)
                                (λ (x)
                                  (repeat (begin0 x (set! x (not x))) until (true? x)))])
                 (||
                  (expt-b #false)
                  (< (expt-i 10) 11)))))))))
   "repeated code")
  
  (translate/test
   (let ([start : Integer 42424242424242424])
     ;; silly functions that should be baked in 
     (letrec ([true? : (Boolean -> Boolean) (λ (x) x)])
       (letrec ([not : (Boolean -> Boolean) (λ (x) (if x #false #true))])
         (letrec ([zero? : (Integer -> Boolean) (λ (x) (= x 0))])
           (letrec ([sub1 : (Integer -> Integer) (λ (x) (- x 1))])
             ;; the 'repeated code'
             (let ()
               (: expt (∀ (α) ((α -> α) -> ((α -> Boolean) -> (α -> α)))))
               (define (expt {exec : (α -> α)})
                 (letrec ([expt-2 : ((α -> Boolean) -> (α -> α))
                                  (λ (stop?)
                                    (letrec ([expt-3 : (α -> α)
                                                     (λ (x) 
                                                       (repeat (begin0 x (set! x (exec x))) : α
                                                               until (stop? x)))])
                                      expt-3))])
                   expt-2))
               (|| 
                ((((inst expt Boolean) not) true?) #false)
                (< ((((inst expt Integer) sub1) zero?) 10) 11))))))))
   "parametric function")
  
  (translate/test
   (let ()
     (: f (∀ (β) (β -> β)))
     (define (f {g :  β}) g)
     (let ()
       (: g (∀ (α) (α -> α)))
       (define (g {x : α}) x)
       (let ([x : Integer 1])
         (repeat (begin0 x (set! x [(inst (if (= x 0) f g) Integer) 0])) : Integer until (= x 0)))))

   "parametric is first-class"))

