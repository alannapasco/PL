#! /bin/sh
#| -*- racket -*-
exec racket -tm "$0" -- ${1+"$@"}
|#
#lang typed/racket

(provide main)

;; -----------------------------------------------------------------------------
(require typed/json)
(module+ test (require "../../Git/Lib/test-framework.rkt"))

;; -----------------------------------------------------------------------------
;; interpret a JSON representation of simple Boolean expressions 

(define (main)
  (define input  : (U EOF JSExpr)  (read-json))
  (cond [(eof-object? input) (void)] [else (write-json (value input)) (newline)]))

(: value (-> JSExpr Boolean))
;; determine the standard meaning of b
(define (value b)
  (match b
    [`(,left "^"  ,right) (and (value left) (value right))]
    [`(,left "||" ,right) (or (value left) (value right))]
    [#t #t]
    [#f #f]
    [_ (error 'value "unexpected input: ~a" (jsexpr->string b))]))

;; -----------------------------------------------------------------------------
;; unit tests 
(module+ test
  (test main '(#true "^" #false)  #false "and false")
  (test main '(#true "||" #false) #true  "or true")
  (test main '(#true "|" #false)  #false "exn" exn:fail?))
