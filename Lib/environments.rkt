#lang typed/racket

(provide Env new-env env-add env-retrieve)

(define-type [Env α] (Listof [List Symbol α]))

(: new-env (All (α) (-> [Env α])))
(define (new-env)
  '[])
  
(: env-add (All (α) (-> [Env α] Symbol α [Env α])))
(define (env-add env x m)
  (cons [list x m] env))

(: env-retrieve (All (α) (-> [Env α] Symbol String α)))
(define (env-retrieve env x fmt)
  (define v (assq x env))
  (unless v (error 'value fmt x))
  (second v))