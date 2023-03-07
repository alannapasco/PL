#lang typed/racket

(let ()
  (define a%
    (class object%
      (super-new)
      (field (x : Integer 10))
      (: m (Integer -> Integer))
      (define/public (m y) (begin0 x (set-field! x this y)))))

  (define b%
    (class object%
      (super-new)
      (field (x : Boolean #false))
      (: k (Boolean -> Boolean))
      (define/public (k z)
        (begin0 x (set-field! x this (or x z))))))
  
  (let ([c (new a%)])
    (let ([d (new b%)])
      (let ([_ : Boolean (send d k #true)])
        (and (get-field x d)
             (< (+ (send c m 42) (get-field x c)) 100))))))
