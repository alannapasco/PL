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
  (define tcd (tc ast))
  (define out (value (cdr tcd)))
  out)

;; -----------------------------------------------------------------------------
;; parsing JSExpr to make sure they are Boolean JSExpr

(define-type E-AST (U Boolean [List '&& E-AST E-AST] [List '|| E-AST E-AST]
                      Integer [List '++ E-AST E-AST] [List '-- E-AST E-AST]
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
;; type check AST and embedd knowledge of types into AST 

(define-type Type (U 'int 'bool))

(define-type B-AST (U Boolean
                      (List '&& B-AST B-AST)
                      (List '|| B-AST B-AST)
                      (List '<< I-AST I-AST)))

(define-type I-AST (U Integer
                      (List '++ I-AST I-AST)
                      (List '-- I-AST I-AST)))

(define-type T-AST (U I-AST B-AST))

(: tc (AST -> (cons Type T-AST)))
(define (tc b)
  (match b
    [(? boolean? b) (cons 'bool b)]
    [`(&& ,left  ,right)
     (match (tc left)
       [(cons 'bool tleft)
        (match (tc right)
          [(cons 'bool tright)
           ((inst cons Type B-AST) 'bool `(&& ,(cast tleft B-AST)  ,(cast tright B-AST)))]
          [_ (error 'tc "and right")])]
       [_ (error 'tc "and left")])]
    [`(|| ,left  ,right)
     (match (tc left)
       [(cons 'bool tleft)
        (match (tc right)
          [(cons 'bool tright)
           ((inst cons Type B-AST) 'bool `(|| ,(cast tleft B-AST)  ,(cast tright B-AST)))]
          [_ (error 'tc "or right")])]
       [_ (error 'tc "or left")])]
    [(? exact-integer? b) (cons 'int b)]
    [`(++ ,left  ,right)
     (match (tc left)
       [(cons 'int tleft)
        (match (tc right)
          [(cons 'int tright)
           ((inst cons Type I-AST) 'int `(++ ,(cast tleft I-AST)  ,(cast tright I-AST)))]
          [_ (error 'tc "++ right")])]
       [_ (error 'tc "++ left")])]
    [`(-- ,left  ,right)
     (match (tc left)
       [(cons 'int tleft)
        (match (tc right)
          [(cons 'int tright)
           ((inst cons Type I-AST) 'int `(-- ,(cast tleft I-AST)  ,(cast tright I-AST)))]
          [_ (error 'tc "-- right")])]
       [_ (error 'tc "-- left")])]
    [`(<< ,left  ,right)
     (match (tc left)
       [(cons 'int tleft)
        (match (tc right)
          [(cons 'int tright)
           ((inst cons Type B-AST) 'bool `(<< ,(cast tleft I-AST)  ,(cast tright I-AST)))]
          [_ (error 'tc "-- right")])]
       [_ (error 'tc "-- left")])]))
                      
;; -----------------------------------------------------------------------------
;; determine the standard meaning of b

(: value (-> T-AST Meaning))
(define (value b)
  (match b
    [(? boolean? b)      (b-value b)]
    [`(&& ,left  ,right) (b-value b)]
    [`(|| ,left ,right)  (b-value b)]
    [(? exact-integer? b) (i-value b)]
    [`(++ ,left  ,right) (i-value b)]
    [`(-- ,left  ,right) (i-value b)]
    [`(<< ,left  ,right) (b-value b)]))

(: i-value (I-AST -> Integer))
(define (i-value b)
  (match b
    [(? exact-integer? b) b]
    [`(++ ,left  ,right) (+ (i-value left) (i-value right))]
    [`(-- ,left  ,right) (- (i-value left) (i-value right))]))

(: b-value (B-AST -> Boolean))
(define (b-value b)
  (match b
    [(? boolean? b) b]
    [`(&& ,left  ,right) (and (b-value left) (b-value right))]
    [`(|| ,left ,right)  (or (b-value left) (b-value right))]
    [`(<< ,left  ,right) (< (i-value left) (i-value right))]))

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