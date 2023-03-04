#lang typed/racket

(let ([start : Integer 42424242424242424])
  ;; silly functions that should be baked in 
  (letrec ([true? : (-> Boolean Boolean) (λ (x) x)])
    (letrec ([not : (-> Boolean Boolean) (λ (x) (if x false true))])
      (letrec ([zero? : (-> Integer Boolean) (λ (x) (= x 0))])
        (letrec ([sub1 : (-> Integer Integer) (λ (x) (- x 1))])
          ;; the 'repeated code'
          (letrec ([expt-i : (Integer -> Integer)
                           (λ (x)
                             (repeat (begin0 x (set! x (sub1 x))) : Integer
                                     until (zero? x)))])
            (letrec ([expt-b : (Boolean -> Boolean)
                             (λ (x)
                               (repeat (begin0 x (set! x (not x))) : Boolean
                                       until (true? x)))])
              (or 
               (expt-b false)
               (< (expt-i 10) 11)))))))))

(define-syntax-rule (repeat stmt : α until e)
  (let loop : α ()
    (define v stmt)
    (if e v (loop))))

(let ([start : Integer 42424242424242424])
  ;; silly functions that should be baked in 
  (letrec ([true? : (-> Boolean Boolean) (λ (x) x)])
    (letrec ([not : (-> Boolean Boolean) (λ (x) (if x false true))])
      (letrec ([zero? : (-> Integer Boolean) (λ (x) (= x 0))])
        (letrec ([sub1 : (-> Integer Integer) (λ (x) (- x 1))])
          ;; the 'repeated code'
          (let ()
            (: expt (∀ (α) ((α -> α) -> ((α -> Boolean) -> (α -> α)))))
            (define (expt {exec : (-> α α)})
              (letrec ([expt-2 : ((-> α Boolean) -> (α -> α))
                               (λ (stop?)
                                 (letrec ([expt-3 : (α -> α)
                                                  (λ (x) 
                                                    (repeat (begin0 x (set! x (exec x))) : α
                                                            until (stop? x)))])
                                   expt-3))])
                expt-2))
            (or 
             ((((inst expt Boolean) not) true?) false)
             (< ((((inst expt Integer) sub1) zero?) 10) 11))))))))

