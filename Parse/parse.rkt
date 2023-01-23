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

(define-type Meaning (U Boolean))

(: pipe (JSExpr -> Meaning))
(define (pipe input)
  (define ast (parse input))
  (define out (value ast))
  out)

;; -----------------------------------------------------------------------------
;; parsing JSExpr to make sure they are Boolean JSExpr

(define-type E-AST (U Boolean [List '&& E-AST E-AST] [List '|| E-AST E-AST] 'ERR))

(define-type AST (U Boolean [List '&& AST AST] [List '|| AST AST]))

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
    [_ 'ERR]))

;; -----------------------------------------------------------------------------
;; determine the standard meaning of b

(: value (-> AST Meaning))
(define (value b)
  (match b
    [`(&& ,left  ,right) (and (value left) (value right))]
    [`(|| ,left ,right)  (or (value left) (value right))]
    [#t #t]
    [#f #f]))

;; -----------------------------------------------------------------------------
;; -----------------------------------------------------------------------------
;; integration and unit tests

(module+ test ;; integration tests for `main`
  (test main '(#true "^" #false)  #false "and false")
  (test main '(#true "||" #false) #true  "or true")
  (test main '(#true "|" #false)  #false "exn" exn:fail?))

(module+ test ;; unit tests for `parse`
  (check-equal? (parse '(#true "^" #false))  '[&& #t #f] "parse and false")
  (check-equal? (parse '(#true "||" #false)) '[|| #t #f] "parse or true")
  (check-exn exn:fail? (λ () (parse '(#true "|" #false))) "exn"))