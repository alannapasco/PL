#lang typed/racket

(provide Env new-env env-add env-rec env-retrieve)

(define-type [Env α] (Listof (U [List Symbol 'plain α] [List Symbol 'rec α [Env α]])))

(: new-env (All (α) (-> [Env α])))
(define (new-env)
  '[])
  
(: env-add (All (α) (-> [Env α] Symbol α [Env α])))
(define (env-add env x m)
  (cons [list x 'plain m] env))

(: env-rec (All (α) (-> [Env α] Symbol α [Env α])))
(define (env-rec env x m)
  (cons [list x 'rec m env] env))

(: env-retrieve (All (α) (->* [[Env α] Symbol String] ([-> Symbol α [Env α] α]) α)))
(define (env-retrieve env x fmt (f (λ (s a e) (error 'value fmt x))))
  (define v (assq x env))
  (match v
    [#false              (error 'value fmt x)]
    [(list x 'plain m)   m]
    [(list x 'rec m env) (f x m env)]))
