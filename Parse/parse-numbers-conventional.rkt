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
;; interpret a JSON representation of simple Boolean expressions 

(define (main)
  (define input  : (U EOF JSExpr)  (read-json))
  (cond [(eof-object? input) (void)] [else (write-json (pipe input)) (newline)]))

(define-type Meaning (U Boolean Integer))

(: pipe (JSExpr -> Meaning))
(define (pipe input)
  (define ast (parse input))
  (define ___ (tc ast))
  (define out (value ast))
  out)

;; -----------------------------------------------------------------------------
;; parsing JSExpr to make sure they are Boolean JSExpr

(define-type E-AST (U Boolean
                      [List '&& E-AST E-AST]
                      [List '|| E-AST E-AST]

                      Integer
                      [List '++ E-AST E-AST]
                      [List '-- E-AST E-AST]
                      [List '<< E-AST E-AST]
                      
                      'ERR))

(define-type AST (U Boolean [List '&& AST AST] [List '|| AST AST]
                    Integer [List '++ AST AST] [List '-- AST AST]
                    [List '<< AST AST] ))

(: parse (JSExpr -> AST))
(define (parse j)
  (define q (e-parse j))
  (with-handlers ([exn:fail? (λ _ (error 'parse "Boolean JSExpr expected, given ~e" q))])
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
    [_ 'ERR]))

;; -----------------------------------------------------------------------------
;; type check AST 

(define-type Type (U 'int 'bool))

(: tc (AST -> Type))
(define (tc b)
  (match b
    [(? boolean? b) 'bool]
    [`(,(or '|| '&&) ,left  ,right)
     (match* ((tc left) (tc right))
       [('bool 'bool) 'bool]
       [(_ _) (error 'tc "&& or ||")])]
    [(? exact-integer? b) 'int]
    [`(,(or '++ '--) ,left  ,right)
     (match* ((tc left) (tc right))
       [('int 'int) 'int]
       [(_ _) (error 'tc "++ or --")])]
    [`(<< ,left  ,right)
     (match* ((tc left) (tc right))
       [('int 'int) 'bool]
       [(_ _) (error 'tc "<<")])]))
                      
;; -----------------------------------------------------------------------------
;; determine the standard meaning of b

(define-syntax-rule (bcast e) (cast e Boolean))
(define-syntax-rule (icast e) (cast e Integer))

(: value (-> AST Meaning))
(define (value b)
  (match b
    [(? exact-integer? b) b]
    [(? boolean? b) b]
    [`(&& ,left  ,right) (and (bcast (value left)) (bcast (value right)))]
    [`(|| ,left ,right)  (or  (bcast (value left)) (bcast (value right)))]
    [(? exact-integer? b) b]
    [`(++ ,left  ,right) (+ (icast (value left)) (icast (value right)))]
    [`(-- ,left  ,right) (- (icast (value left)) (icast (value right)))]
    [`(<< ,left  ,right) (< (icast (value left)) (icast (value right)))]))

;; -----------------------------------------------------------------------------
;; -----------------------------------------------------------------------------
;; integration and unit tests

(module+ test ;; integration tests for `main`
  (test main '(#true "^" #false)  #false "and false")
  (test main '(#true "||" #false) #true  "or true")
  (test main '(#true "|" #false)  #false "exn" exn:fail?)
  (test main '(5 "+" 0)  5 "numbers")
  (test main '(((5 "+" 0) "<" 10) "^" (#false "||" #true))  #true "complex"))

(module+ test ;; unit tests for `parse`
  (check-equal? (parse '(#true "^" #false))  '[&& #t #f] "parse and false")
  (check-equal? (parse '(#true "||" #false)) '[|| #t #f] "parse or true")
  (check-exn exn:fail? (λ () (parse '(#true "|" #false))) "exn"))